{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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

syscallRead = I 0
syscallWrite = I 1

stdinFd = I 0
stdoutFd = I 1
stderrFd = I 2

class (Functor f, Monad m) => Compile m f where
    compileArg :: f (m r) -> m r

instance (Compile m f, Compile m g) => Compile m (f :+: g) where
    compileArg (InL f) = compileArg f
    compileArg (InR g) = compileArg g

instance Compile (AsmF addr) Fuck.GoLeft where
    compileArg (Fuck.GoLeft m) = do
        dec rcx
        m

instance Compile (AsmF addr) Fuck.GoRight where
    compileArg (Fuck.GoRight m) = do
        inc rcx
        m

instance Compile (AsmF addr) Fuck.Inc where
    compileArg (Fuck.Inc m) = do
        inc ircx
        m

instance Compile (AsmF addr) Fuck.Dec where
    compileArg (Fuck.Dec m) = do
        dec ircx
        m

instance Compile (AsmF addr) f => Compile (AsmF addr) (Fuck.Loop f) where
    compileArg (Fuck.Loop body m) = do
        l <- label
        _ <- foldFM compileArg body
        loop (A l)
        m

instance Compile (AsmF addr) Fuck.Output where
    compileArg (Fuck.Output m) = do
        mov irsp rcx
        mov rax syscallWrite
        mov rdi stdoutFd
        mov rsi rcx -- buffer to write from
        mov rdx (I 1) -- number of bytes to write
        syscall
        mov rcx irsp -- restore rcx in case the syscall messed with it
        m

instance Compile (AsmF addr) Fuck.Input where
    compileArg (Fuck.Input m) = do
        mov rax syscallRead
        mov rdi stdinFd
        mov rsi rcx -- buffer to read into
        mov rdx (I 1) -- number of bytes to read
        syscall
        mov rcx irsp -- restore rcx in case the syscall messed with it
        m

instance Compile (AsmF addr) FuckDSL where
    compileArg (FuckDSL f) = compileArg f

-- | Wraps assembly code with the function intro and outro logic to deal with
-- stack frames.
asmFunction :: AsmF addr () -> AsmF addr ()
asmFunction body = do
    push rbp
    mov rbp rsp
    body
    mov rsp rbp
    pop rbp
    ret

-- | Raw compilation from brainfuck to assembly.
compileFuck :: Free FuckDSL () -> AsmF addr ()
compileFuck = foldFM compileArg
