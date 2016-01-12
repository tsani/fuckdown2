{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module Memory where

import Foreign
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe ( unsafeUseAsCString )
import Data.Word

import Mprotect
import Invoke

foreign import ccall unsafe "sys/mman.h mmap"
    c_mmap :: Ptr a -> Int -> Int -> Int -> Int -> Int -> IO (Ptr b)

foreign import ccall unsafe "sys/mman.h munmap"
    c_munmap :: Ptr a -> Int -> IO Int

mmapAnonymousSize :: Int -> Maybe [Protection] -> IO (Ptr a)
mmapAnonymousSize sz (storeProtection -> prot)
    = c_mmap nullPtr sz prot flagMapAnonymous (-1) 0 where
        flagMapAnonymous = 0x20

-- | Unsafely extracts the byte pointer of a ByteString, makes its memory
-- region executable, and synthesizes an IO action representing running the
-- code there.
byteStringFunction :: BS.ByteString -> IO (Ptr a -> IO ())
byteStringFunction bs = do
    let l = BS.length bs
    p <- unsafeUseAsCString bs $ \(castPtr -> p) -> do
        m <- mmapAnonymousSize l (Just [ProtRead, ProtWrite, ProtExec])
        copyBytes m p l
        return m
    mkInvoke p
