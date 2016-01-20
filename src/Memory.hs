{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module Memory
( mmapAnonymousSize
, byteStringFunction
, c_mmap
, c_munmap
) where

import Foreign
import Foreign.C
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe ( unsafeUseAsCString )

import Mprotect
import Invoke

foreign import ccall unsafe "sys/mman.h mmap"
    c_mmap :: Ptr a -> Word64 -> Word32 -> Int -> Int -> Int -> IO (Ptr b)

foreign import ccall unsafe "sys/mman.h munmap"
    c_munmap :: Ptr a -> Int -> IO Int

-- | Create an anonymous mapping of bytes of a given size with given memory
-- protection constraints.
mmapAnonymousSize :: Word64 -> Maybe [Protection] -> IO (Ptr a)
mmapAnonymousSize sz (storeProtection -> prot) = do
    let flagMapAnonymous = 0x20
        flagMapPrivate = 0x02
    throwErrnoIf ((== (-1)) . ptrToIntPtr) "mmapAnonymous" $ do
        c_mmap nullPtr sz prot (flagMapAnonymous .|. flagMapPrivate) (-1) 0

-- | Unsafely extracts the byte pointer of a ByteString, makes its memory
-- region executable, and synthesizes an IO action representing running the
-- code there.
byteStringFunction :: BS.ByteString -> IO (Ptr a -> IO ())
byteStringFunction bs = do
    let l = BS.length bs
    p <- unsafeUseAsCString bs $ \(castPtr -> p) -> do
        m <- mmapAnonymousSize
                 (fromIntegral l)
                 (Just [ProtRead, ProtWrite, ProtExec])
        copyBytes m p l
        return m
    mkInvoke p
