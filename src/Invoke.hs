{-# LANGUAGE ForeignFunctionInterface #-}

module Invoke
( mkInvoke
) where

import Data.Word
import Foreign
import Unsafe.Coerce ( unsafeCoerce )

foreign import ccall "dynamic"
    mkFun :: FunPtr (IO ()) -> IO ()

-- | Generates an IO action that will execute the given pointer as machine
-- code. In order to be \"well-behaved\", the machine code should implement a
-- function follow the C x86-64 calling convention.
mkInvoke :: Ptr Word8 -> IO (IO ())
mkInvoke mem = do
    let fp = unsafeCoerce mem :: FunPtr (IO ())
    return $ mkFun fp
