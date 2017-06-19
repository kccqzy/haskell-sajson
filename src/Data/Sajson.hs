{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Sajson
  ( sajsonParse
  , SajsonParseError(..)
  ) where

import Control.Exception
import qualified Data.ByteString as B
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data SajsonDocument

data SajsonParseError
  = SajsonParseError
  { sajsonParseErrorLine :: Int
  , sajsonParseErrorColumn :: Int
  , sajsonParseErrorMessage :: String
  } deriving (Show, Eq)

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

sajsonParse :: B.ByteString -> IO (Either SajsonParseError ())
sajsonParse bs = B.useAsCStringLen bs $ \(ptr, size) ->
  bracket (c_sajson_parse_single_allocation ptr (fromIntegral size)) c_sajson_free_document $ \ doc -> do
  hasError <- c_sajson_has_error doc
  case hasError of
    0 -> pure $ Right ()
    _ -> Left <$> (SajsonParseError
                   <$> (fromIntegral <$> c_sajson_get_error_line doc)
                   <*> (fromIntegral <$> c_sajson_get_error_column doc)
                   <*> (c_sajson_get_error_message doc >>= peekCString))
