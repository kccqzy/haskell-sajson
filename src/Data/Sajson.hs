{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Sajson
  ( sajsonParse
  , SajsonParseError(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- | Phantom type for a pointer to sajson_document.
data SajsonDocument

data SajsonParseError
  = SajsonParseError
  { sajsonParseErrorLine :: Int
  , sajsonParseErrorColumn :: Int
  , sajsonParseErrorMessage :: String
  } deriving (Show, Eq)

type SajsonValueType = CUChar
type SajsonValuePayload = Ptr CSize
type SajsonValueInputMutableView = Ptr CUChar

foreign import ccall unsafe "sajson_wrapper.h sajson_parse_single_allocation"
  c_sajson_parse_single_allocation :: Ptr CChar -> CSize -> IO (Ptr SajsonDocument)

foreign import ccall unsafe "sajson_wrapper.h sajson_free_document"
  c_sajson_free_document :: Ptr SajsonDocument -> IO ()

foreign import ccall unsafe "sajson_wrapper.h sajson_has_error"
  c_sajson_has_error :: Ptr SajsonDocument -> IO CInt

foreign import ccall unsafe "sajson_wrapper.h sajson_get_error_line"
  c_sajson_get_error_line :: Ptr SajsonDocument -> IO CSize

foreign import ccall unsafe "sajson_wrapper.h sajson_get_error_column"
  c_sajson_get_error_column :: Ptr SajsonDocument -> IO CSize

foreign import ccall unsafe "sajson_wrapper.h sajson_get_error_message"
  c_sajson_get_error_message :: Ptr SajsonDocument -> IO CString

foreign import ccall unsafe "sajson_wrapper.h sajson_get_root_type"
  c_sajson_get_root_type :: Ptr SajsonDocument -> IO SajsonValueType

foreign import ccall unsafe "sajson_wrapper.h sajson_get_root"
  c_sajson_get_root :: Ptr SajsonDocument -> IO SajsonValuePayload

foreign import ccall unsafe "sajson_wrapper.h sajson_get_input"
  c_sajson_get_input :: Ptr SajsonDocument -> IO SajsonValueInputMutableView

sajsonParse :: B.ByteString -> IO (Either SajsonParseError Value)
sajsonParse bs = B.useAsCStringLen bs $ \(ptr, size) ->
  bracket (c_sajson_parse_single_allocation ptr (fromIntegral size)) c_sajson_free_document $ \ doc -> do
  hasError <- c_sajson_has_error doc
  case hasError of
    0 -> Right <$> join (constructHaskellValue
                         <$> c_sajson_get_root_type doc
                         <*> c_sajson_get_root doc
                         <*> c_sajson_get_input doc)
    _ -> Left <$> (SajsonParseError
                   <$> (fromIntegral <$> c_sajson_get_error_line doc)
                   <*> (fromIntegral <$> c_sajson_get_error_column doc)
                   <*> (c_sajson_get_error_message doc >>= peekCString))

makeString :: SajsonValuePayload -> SajsonValueInputMutableView -> IO T.Text
makeString payload buf = do
  start <- peekElemOff payload 0
  end <- peekElemOff payload 1
  bs <- B.packCStringLen (buf `plusPtr` fromIntegral start, fromIntegral (end - start))
  pure (TE.decodeUtf8 bs)

makeValue :: SajsonValuePayload -> Int -> SajsonValueInputMutableView -> IO Value
makeValue payload index buf = do
    element <- peekElemOff payload index
    let vt = fromIntegral (element .&. 7)
        vp = payload `plusPtr` fromIntegral (8 * (element `shiftR` 3)) -- where 8 == sizeof(size_t)
    constructHaskellValue vt vp buf

constructHaskellValue :: SajsonValueType -> SajsonValuePayload -> SajsonValueInputMutableView -> IO Value
constructHaskellValue 0 payload _ = do -- TYPE_INTEGER
  i <- peekElemOff (castPtr payload) 0
  pure (Number (fromIntegral (i :: CInt)))
constructHaskellValue 1 payload _ = do -- TYPE_DOUBLE
  d <- peekElemOff (castPtr payload) 0
  pure (Number (fromFloatDigits (d :: CDouble)))
constructHaskellValue 2 _ _ = -- TYPE_NULL
  pure Null
constructHaskellValue 3 _ _ = -- TYPE_FALSE
  pure (Bool False)
constructHaskellValue 4 _ _ = -- TYPE_TRUE
  pure (Bool True)
constructHaskellValue 5 payload buf = -- TYPE_STRING
  String <$> makeString payload buf
constructHaskellValue 6 payload buf = do -- TYPE_ARRAY
  len <- peekElemOff payload 0
  (Array <$>) . V.generateM (fromIntegral len) $ \i -> makeValue payload (1 + i) buf
constructHaskellValue 7 payload buf = do -- TYPE_OBJECT
  len <- peekElemOff payload 0
  Object <$> foldrM go HM.empty [1 .. len] -- do not use [0..len-1]: integer overflow
  where go i' hm = do
          let i = i' - 1
          key <- makeString (payload `plusPtr` fromIntegral (8 * (1 + i * 3))) buf
          val <- makeValue payload (fromIntegral (3 + i * 3)) buf
          pure $ HM.insert key val hm
constructHaskellValue vt _ _ = throwIO (ErrorCall $ "Data.Sajson internal error: unexpected sajson type tag: " ++ show vt)
