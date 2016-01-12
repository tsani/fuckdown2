{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module Mprotect where

import Data.Bits
import Data.Word
import Foreign
import Foreign.C.Error
import Foreign.C.Types

foreign import ccall unsafe "sys/mman.h mprotect"
    c_mprotect :: Ptr a -> Word64 -> Word32 -> IO Int

data Protection
    = ProtRead
    | ProtWrite
    | ProtExec

protRead = 0x1
protWrite = 0x2
protExec = 0x4
protNone = 0x0

protVal :: Protection -> Int
protVal ProtRead = protRead
protVal ProtWrite = protWrite
protVal ProtExec = protExec

storeProtection :: Maybe [Protection] -> Int
storeProtection Nothing = protNone
storeProtection (Just ps) = foldr (.|.) protNone (map protVal ps)

mprotectSize :: Ptr a -> Int -> Maybe [Protection] -> IO ()
mprotectSize p sz (storeProtection -> ps)
    = throwErrnoIf_ (/= 0) "mprotect"
    $ c_mprotect p (fromIntegral sz) (fromIntegral ps)

mprotect :: Storable a => Ptr a -> Maybe [Protection] -> IO ()
mprotect p mps = do
    x <- peek p
    mprotectSize p (sizeOf x) mps
