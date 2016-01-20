{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module Mprotect
( Protection(..)
, protVal
, storeProtection
) where

import Data.Bits
import Data.Word

data Protection
    = ProtRead
    | ProtWrite
    | ProtExec

-- | Read permissions.
protRead :: Word32
protRead = 0x1

-- | Write permissions.
protWrite :: Word32
protWrite = 0x2

-- | Executable permissions.
protExec :: Word32
protExec = 0x4

-- | No protection.
protNone :: Word32
protNone = 0x0

protVal :: Protection -> Word32
protVal ProtRead = protRead
protVal ProtWrite = protWrite
protVal ProtExec = protExec

storeProtection :: Maybe [Protection] -> Word32
storeProtection Nothing = protNone
storeProtection (Just ps) = foldr (.|.) protNone (map protVal ps)
