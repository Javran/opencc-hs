{-|
Module      : Text.OpenCC.Raw
Description : Raw bindings to libopencc
License     : MIT
Stability   : Experimental
Portability : POSIX

This module defines the raw bindings to the OpenCC library. Currently,
`opencc_convert_utf8_to_buffer` is missing.
-}

module Text.OpenCC.Raw
  ( RawOpenCC
  , _openccOpen
  , _openccClose
  , _openccClosePtr
  , _openccConvertUtf8
  , _openccConvertUtf8Free
  , _openccConvertUtf8FreePtr
  , _openccError
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

-- |Raw representation of the OpenCC handle.
type RawOpenCC = Ptr ()

foreign import ccall "opencc_open" _openccOpen :: CString -> IO RawOpenCC
foreign import ccall "opencc_close" _openccClose :: RawOpenCC -> IO ()
foreign import ccall "&opencc_close" _openccClosePtr :: FunPtr (RawOpenCC -> IO ())
foreign import ccall "opencc_convert_utf8" _openccConvertUtf8 :: RawOpenCC -> CString -> CSize -> IO CString
foreign import ccall "opencc_convert_utf8_free" _openccConvertUtf8Free :: CString -> IO ()
foreign import ccall "&opencc_convert_utf8_free" _openccConvertUtf8FreePtr :: FunPtr (CString -> IO ())

-- |Return value is `const char *`, do NOT free!
foreign import ccall "opencc_error" _openccError :: IO CString
