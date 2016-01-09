module JIT where

import Data.Binary.Put

class Functor f => JIT f where
    jitCompileArg :: f () -> Free Asm ()
