{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Sajson (sajsonParse) where

import qualified Data.ByteString as B
import Foreign.C.Types
import Foreign.Ptr

data SajsonDocument

foreign import ccall unsafe "sajson_wrapper.h sajson_parse_single_allocation"
  c_sajson_parse_single_allocation :: Ptr CChar -> CSize -> IO (Ptr SajsonDocument)

foreign import ccall unsafe "sajson_wrapper.h sajson_has_error"
  c_sajson_has_error :: Ptr SajsonDocument -> IO CInt

sajsonParse :: B.ByteString -> IO Bool
sajsonParse bs = B.useAsCStringLen bs $ \(ptr, size) -> do
  doc <- c_sajson_parse_single_allocation ptr (fromIntegral size)
  hasError <- c_sajson_has_error doc
  pure (hasError == 0)
