{-# LANGUAGE DeriveFunctor #-}

module Asm where

import Data.Int
import Data.Binary.Put
import Data.Word

import Free

type Immediate = Int64
type Address = Word64

data Val addr
    = I Immediate
    | R Register
    | IR Register
    | A addr
    deriving (Eq, Show)

data Register
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rbp
    | Rsi
    | Rdi
    | Rsp
    deriving (Eq, Show)

-- | High-level assembly code instructions, without concern for addressing
-- modes or other details.
--
-- The address type is left as a parameter to allow for to use whatever address
-- representation might be most useful to them.
data Asm addr f
    = Ret f
    | Mov (Val addr) (Val addr) f
    | Add (Val addr) (Val addr) f
    | Sub (Val addr) (Val addr) f
    | Mul (Val addr) f
    | IMul (Val addr) f
    | Xor (Val addr) (Val addr) f
    | Inc (Val addr) f
    | Dec (Val addr) f
    | Push (Val addr) f
    | Pop (Val addr) f
    | Jmp (Val addr) f
    | Loop (Val addr) f
    | Nop f
    | Syscall f
    | Label (addr -> f)
    deriving (Functor)

type AsmF addr = Free (Asm addr)

-- Shorthands for Free-wrapped assembly commands.

ret = liftF . Ret $ ()
mov v1 v2 = liftF . Mov v1 v2 $ ()
add v1 v2 = liftF . Add v1 v2 $ ()
sub v1 v2 = liftF . Sub v1 v2 $ ()
mul v1 = liftF . Mul v1 $ ()
imul v1 = liftF . IMul v1 $ ()
xor v1 v2 = liftF . Xor v1 v2 $ ()
inc v1 = liftF . Inc v1 $ ()
dec v1 = liftF . Dec v1 $ ()
push v1 = liftF . Push v1 $ ()
pop v1 = liftF . Pop v1 $ ()
jmp v1 = liftF . Jmp v1 $ ()
loop v1 = liftF . Loop v1 $ ()
nop = liftF . Nop $ ()
syscall = liftF . Syscall $ ()
label = liftF . Label $ id

-- Shorthands for registers

rax :: Val addr
rax = R Rax

rbx :: Val addr
rbx = R Rbx

rcx :: Val addr
rcx = R Rcx

rdx :: Val addr
rdx = R Rdx

rbp :: Val addr
rbp = R Rbp

rsi :: Val addr
rsi = R Rsi

rdi :: Val addr
rdi = R Rdi

rsp :: Val addr
rsp = R Rsp

-- Shorthands for indirect register addressing

irax :: Val addr
irax = IR Rax

irbx :: Val addr
irbx = IR Rbx

ircx :: Val addr
ircx = IR Rcx

irdx :: Val addr
irdx = IR Rdx

irbp :: Val addr
irbp = IR Rbp

irsp :: Val addr
irsp = IR Rsp

irsi :: Val addr
irsi = IR Rsi

irdi :: Val addr
irdi = IR Rdi

-- | Gets the index of a register.
index :: Num a => Register -> a
index r = case r of
    Rax -> 0
    Rcx -> 1
    Rdx -> 2
    Rbx -> 3
    Rsp -> 4
    Rbp -> 5
    Rsi -> 6
    Rdi -> 7

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
