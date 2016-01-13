{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Compiler
( asmFunction
, compileFuck
, Compile(..)
) where

import Asm
import Coproduct
import DSL
import Free
import qualified Fuckdown as Fuck

-- | An assembly value representing an immediate containing the number of the
-- /read/ syscall.
syscallRead :: Val label addr
syscallRead = I 0

-- | An assembly value representing an immediate containing the number of the
-- /write/ syscall.
syscallWrite :: Val label addr
syscallWrite = I 1

-- | An assembly value representing an immediate containing the file descriptor
-- number of standard in.
stdinFd :: Val label addr
stdinFd = I 0

-- | An assembly value representing an immediate containing the file descriptor
-- number of standard out.
stdoutFd :: Val label addr
stdoutFd = I 1

-- | An assembly value representing an immediate containing the file descriptor
-- number of standard error.
stderrFd :: Val label addr
stderrFd = I 2

class (Functor f, Monad m) => Compile m f where
    compileArg :: f (m r) -> m r

instance (Compile m f, Compile m g) => Compile m (f :+: g) where
    compileArg (InL f) = compileArg f
    compileArg (InR g) = compileArg g

instance Compile (AsmF label addr) Fuck.GoLeft where
    compileArg (Fuck.GoLeft m) = do
        dec rax
        m

instance Compile (AsmF label addr) Fuck.GoRight where
    compileArg (Fuck.GoRight m) = do
        inc rax
        m

instance Compile (AsmF label addr) Fuck.Inc where
    compileArg (Fuck.Inc m) = do
        inc irax
        m

instance Compile (AsmF label addr) Fuck.Dec where
    compileArg (Fuck.Dec m) = do
        dec irax
        m

instance Compile (AsmF label addr) f => Compile (AsmF label addr) (Fuck.Loop f) where
    compileArg (Fuck.Loop body m) = do
        done <- newLabel

        -- if the cell is already zero, jump over the loop body
        cmp irax (I 0)
        je (L done)

        -- set a label for the loop start
        inner <- label
        _ <- foldFM compileArg body
        cmp irax (I 0)
        jne (L inner)
        -- decrement the cell until we get to zero

        setLabel done
        m

instance Compile (AsmF label addr) Fuck.Output where
    compileArg (Fuck.Output m) = do
        push rax
        mov rsi rax -- buffer to write from
        mov rax syscallWrite
        mov rdi stdoutFd
        mov rdx (I 1) -- number of bytes to write
        syscall
        pop rax -- restore rax in case the syscall messed with it
        m

instance Compile (AsmF label addr) Fuck.Input where
    compileArg (Fuck.Input m) = do
        push rax
        mov rax syscallRead
        mov rdi stdinFd
        mov rsi rax -- buffer to read into
        mov rdx (I 1) -- number of bytes to read
        syscall
        pop rax
        m

instance Compile (AsmF label addr) FuckDSL where
    compileArg (FuckDSL f) = compileArg f

-- | Raw compilation from brainfuck to assembly.
compileFuck :: Free FuckDSL () -> AsmF label addr ()
compileFuck = foldFM compileArg
