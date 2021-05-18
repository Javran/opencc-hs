{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, Strict #-}
{-|
Module      : Text.OpenCC
Description : Higher-level bindings to OpenCC
License     : MIT
Stability   : Experimental
Portability : POSIX

This module exposes higher-level binding to OpenCC than
"Text.OpenCC.Raw". OpenCC resources are managed automatically, and
large string objects are passed without being copied.

There are three sets of higher-level bindings.

1. IO handles. You work in 'IO' monad and work with 'OpenCC'
handles directly.
2. One-shot. 'convert1' directly converts a 'String'.
3. Monadic. You work in 'OpenCCM' monad where you can use 'convert'.

__Caveat__: the one-shot interface is unsafe, but as long as OpenCC is
not shared or you don't care about errors, you will be fine.

'defaultTradToSimp' and 'defaultSimpToTrad' are suggested by OpenCC.
-}
module Text.OpenCC
  ( convert1
  , OpenCC, open, lastError, convertIO
  , OpenCCM, withOpenCC, unsafeWithOpenCC, convert
  , defaultSimpToTrad, defaultTradToSimp
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Internal ( fromForeignPtr, c_strlen )
import           Foreign.C.String ( CString, withCString, withCStringLen )
import           Foreign.Ptr ( Ptr, FunPtr, ptrToIntPtr )
import           Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr, castForeignPtr )
import           System.IO.Unsafe ( unsafePerformIO )
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Bits ( complement )
import           Text.OpenCC.Raw

-- |OpenCC handle plus the finalizer. The OpenCC instance will be
-- finalized when the object is garbage collected.
type OpenCC = ForeignPtr ()

-- |Filename of default Simplified to Traditional configuration
defaultSimpToTrad :: String
defaultSimpToTrad = "s2t.json"

-- |Filename of default Traditional to Simplified configuration
defaultTradToSimp :: String
defaultTradToSimp = "t2s.json"

-- |Open a new OpenCC session with specified configuration. 'Nothing'
-- is returned if error happens, and the error message can be
-- retrieved from 'lastError'.
open :: String -> MaybeT IO OpenCC
open cfg = do
  raw <- lift $ withCString cfg _openccOpen
  guard ((ptrToIntPtr raw) /= (complement 0))
  handle <- lift $ newForeignPtr _openccClosePtr raw
  return handle

-- |Use an OpenCC handle to do the conversion. The result is a UTF-8
-- encoded byte string.
convertIO :: OpenCC -> String -> IO BS.ByteString
convertIO handle str = withForeignPtr handle $ \ptr -> withCStringLen str $ \(cstr,len) ->
  _openccConvertUtf8 ptr cstr (fromIntegral len) >>= _wrapBS'

-- |Return the last error message. This function is NOT thread-safe.
lastError :: IO BS.ByteString
lastError = do
  err <- _openccError
  ptr <- newForeignPtr_ err
  len <- c_strlen err
  return $ fromForeignPtr (castForeignPtr ptr) 0 (fromIntegral len)

-- |Do a one-shot conversion. Note that this might affect the outcome
-- of 'lastError', and thus unsafe (despite the pureness suggested by
-- the signature).
--
-- > convert1 defaultSimpToTrad "头发发财"
convert1 :: String -> String -> Maybe BS.ByteString
convert1 cfg str = (unsafePerformIO . runMaybeT) $ do
  handle <- open cfg
  res    <- lift $ convertIO handle str
  return res

-- |The OpenCC environment. In this environment, any conversion
-- happens within a single 'OpenCC' instance created by 'withOpenCC'.
newtype OpenCCM a = OpenCCM (ReaderT OpenCC IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- |Convert a string in the current environment.
convert :: String -> OpenCCM BS.ByteString
convert str = OpenCCM $ do
  handle <- ask
  res    <- lift $ convertIO handle str
  return res

-- |Open an OpenCC environment (which is 'MonadIO'), where 'convert'
-- is available.
withOpenCC :: String -> OpenCCM a -> MaybeT IO a
withOpenCC cfg (OpenCCM inner) = open cfg >>= lift . runReaderT inner

-- |Same as 'withOpenCC' but the result is not 'IO'. This is unsafe,
-- and the same safety conditions as 'convert1' apply here.
unsafeWithOpenCC :: String -> OpenCCM a -> Maybe a
unsafeWithOpenCC cfg = unsafePerformIO . runMaybeT . withOpenCC cfg

-- |Wrap a 'CString' in 'BS.ByteString' with no specified finalizer.
_wrapBS :: FunPtr (CString -> IO ()) -> CString -> IO BS.ByteString
_wrapBS finalizer cstr = do
  len <- c_strlen cstr
  ptr <- newForeignPtr finalizer cstr
  return $ fromForeignPtr (castForeignPtr ptr) 0 (fromIntegral len)

-- |Wrap a 'CString' in 'BS.ByteString' with
-- '_openccConvertUtf8FreePtr' being the finalizer. Useful for
-- wrapping strings from OpenCC.
_wrapBS' :: CString -> IO BS.ByteString
_wrapBS' = _wrapBS (_openccConvertUtf8FreePtr)
