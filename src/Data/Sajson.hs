{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Data.Sajson
-- Copyright:   (c) 2017 Zhouyu Qian
--              (c) 2012-2017 Chad Austin
-- License:     MIT
-- Maintainer:  Zhouyu Qian
--
-- A more efficient C++-based JSON parser.

module Data.Sajson
  ( -- * Introduction
    -- $introduction

    -- * Native APIs
    -- $native
    sajsonParse
  , unsafeMutableByteStringSajsonParse
  , SajsonParseError(..)

    -- * Drop-in Replacements
    -- $replacements
  , eitherDecodeStrict
  , decodeStrict
  , eitherDecode
  , decode
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson.Types (FromJSON (..), Value (..), parseEither, parseMaybe)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

-- $introduction
-- This library is a high-performance parser for JSON. It is an alternative
-- to the native parsers in 'Data.Aeson'.
--
-- Before using this library, please carefully consider whether you actually
-- should use this library. This library is specifically designed for
-- Haskell users who find the performance of the pure-Haskell 'Data.Aeson'
-- parser inadequate. This library focuses on performance, not portability
-- or absolute correctness. You should /only/ use this library if all of the
-- following is true:
--
--   *  You are using a system that is 64-bit.
--   *  You are using a system where the alignment of @double@ is no stricter than
--      that of @size_t@.
--   *  You do not expect to parse floating point numbers beyond the range of
--      `double`.
--   *  You do not expect to parse integers that are longer than 53 bits.
--   *  You do not expect parsing a JSON number will produce a Haskell
--      'Data.Scientific.Scientific' value that exactly represents the
--      mathematical value of the JSON number, or even the closest
--      approximation in IEEE-754 `double`.
--   *  You wish to sacrifice memory consumption for speed.
--   *  You are parsing JSON that is known to be valid.
--
-- When all of the above are true, this library could work for you. Again,
-- please benchmark and show that JSON parsing is indeed a bottleneck before
-- using this library.

-- $native
-- A single function 'sajsonParse' is provided to access the parsing
-- functionality directly. However, you might instead want to use one of the
-- drop-in replacements below.

-- | Phantom type for a pointer to sajson_document.
data SajsonDocument

-- | A structure containing information about parse failures. The error line,
-- column, and messages are from the @sajson@ C++ library.
data SajsonParseError
  = SajsonParseError
  { sajsonParseErrorLine :: Int -- ^ The line where the error occurred.
  , sajsonParseErrorColumn :: Int -- ^ The column where the error occurred.
  , sajsonParseErrorMessage :: String -- ^ The error message. This is a canned message; it is not generated from the input.
  } deriving (Show, Eq)

type SajsonValueType = Word8
type SajsonValuePayload = Ptr CSize
type SajsonValueInputMutableView = Ptr CUChar

foreign import ccall unsafe "sajson_wrapper.h sajson_document_sizeof"
  c_sajson_document_sizeof :: CSize

foreign import ccall unsafe "sajson_wrapper.h sajson_parse_single_allocation"
  c_sajson_parse_single_allocation :: Ptr CChar -> CSize -> Ptr CSize -> Ptr CChar -> IO (Ptr SajsonDocument)

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

-- | Parse a 'CStringLen' into a 'Value' with the possibility of failing with
-- 'SajsonParseError'.
doParse :: CStringLen -> IO (Either SajsonParseError Value)
doParse (ptr, size) =
  allocaBytes (fromIntegral c_sajson_document_sizeof) $ \rvbuf ->
  allocaBytes (8 * size) $ \buf -> do
  doc <- c_sajson_parse_single_allocation ptr (fromIntegral size) buf rvbuf
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
{-# INLINE doParse #-}

-- | Parse a 'B.ByteString' into a 'Value' with the possibility of failing with
-- 'SajsonParseError'.
sajsonParse :: B.ByteString -> IO (Either SajsonParseError Value)
sajsonParse bs = B.useAsCStringLen bs doParse

unsafeMutableByteStringSajsonParse :: B.ByteString -> IO (Either SajsonParseError Value)
unsafeMutableByteStringSajsonParse bs = BU.unsafeUseAsCStringLen bs doParse

makeString :: SajsonValuePayload -> Int -> SajsonValueInputMutableView -> IO T.Text
makeString payload index buf = do
  start <- peekElemOff payload index
  end <- peekElemOff payload (1 + index)
  bs <- B.packCStringLen (buf `plusPtr` fromIntegral start, fromIntegral (end - start))
  pure (TE.decodeUtf8 bs)
{-# INLINE makeString #-}

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
  String <$> makeString payload 0 buf
constructHaskellValue 6 payload buf = do -- TYPE_ARRAY
  len <- peekElemOff payload 0
  (Array <$>) . V.generateM (fromIntegral len) $ \i -> makeValue payload (1 + i) buf
constructHaskellValue 7 payload buf = do -- TYPE_OBJECT
  len <- peekElemOff payload 0
  Object <$> V.foldM' go HM.empty (V.enumFromN 0 (fromIntegral len))
  where go hm i = do
          key <- makeString payload (1 + i * 3) buf
          val <- makeValue payload (3 + i * 3) buf
          pure $ HM.insert key val hm
constructHaskellValue vt _ _ = throwIO (ErrorCall $ "Data.Sajson internal error: unexpected sajson type tag: " ++ show vt)

-- $replacements
-- These functions are (almost) drop-in replacements for the equivalently named
-- functions in 'Data.Aeson', with the caveat that the outermost structure must
-- be either an 'Object' or an 'Array'.

toDataEither :: FromJSON a => Value -> Either String a
toDataEither = parseEither parseJSON

toData :: FromJSON a => Value -> Maybe a
toData = parseMaybe parseJSON

describeSajsonParseError :: SajsonParseError -> String
describeSajsonParseError (SajsonParseError line col err) = "Error in sajson parser: line " ++ show line ++ " column " ++ show col ++ ": " ++ err

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'. If this
-- fails, 'Left' is returned with a 'String' describing the issue.
eitherDecodeStrict :: FromJSON a => B.ByteString -> Either String a
eitherDecodeStrict bs = unsafePerformIO $ either (Left . describeSajsonParseError) toDataEither <$> sajsonParse bs

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'. If this
-- fails, 'Nothing' is returned.
decodeStrict :: FromJSON a => B.ByteString -> Maybe a
decodeStrict bs = unsafePerformIO $ either (pure Nothing) toData <$> sajsonParse bs

-- | Deserialize a JSON value from a lazy 'BL.ByteString'. If this fails, 'Left'
-- is returned with a 'String' describing the issue.
--
-- It should be noted that this function first converts the lazy 'BL.ByteString'
-- into a strict 'B.ByteString'. It does not allow streaming. Therefore it
-- requires at least twice the total size of the 'BL.ByteString' to be in memory.
eitherDecode :: FromJSON a => BL.ByteString -> Either String a
eitherDecode = eitherDecodeStrict . BL.toStrict

-- | Deserialize a JSON value from a lazy 'BL.ByteString'. If this
-- fails, 'Nothing' is returned.
--
-- It should be noted that this function first converts the lazy 'BL.ByteString'
-- into a strict 'B.ByteString'. It does not allow streaming. Therefore it
-- requires at least twice the total size of the 'BL.ByteString' to be in memory.
decode :: FromJSON a => BL.ByteString -> Maybe a
decode = decodeStrict . BL.toStrict
