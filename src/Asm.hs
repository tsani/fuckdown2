{-# LANGUAGE DeriveFunctor #-}

module Asm where

import Data.Int
import Data.Binary.Put
import Data.Word

import Free

type Immediate = Int64
type Address = Word64

data Val
    = I Immediate
    | R Register
    | RR Register
    | A Address
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

data Asm f
    = Ret f
    | Mov Val Val f
    | Add Val Val f
    | Sub Val Val f
    | Mul Val f
    | IMul Val f
    | Xor Val Val f
    | Inc Val f
    | Dec Val f
    | Push Val f
    | Pop Val f
    | Jmp Val f
    | Loop Val f
    | Nop f
    | Syscall f
    | Label (Address -> f)
    deriving (Functor)

type AsmF = Free Asm

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

rax :: Val
rax = R Rax

rbx :: Val
rbx = R Rbx

rcx :: Val
rcx = R Rcx

rdx :: Val
rdx = R Rdx

rbp :: Val
rbp = R Rbp

rsi :: Val
rsi = R Rsi

rdi :: Val
rdi = R Rdi

rsp :: Val
rsp = R Rsp

-- Shorthands for indirect register addressing

irax = RR Rax
irbx = RR Rbx
ircx = RR Rcx
irdx = RR Rdx
irbp = RR Rbp
irsp = RR Rsp
irsi = RR Rsi
irdi = RR Rdi

-- | Gets the index of a register.
index :: Register -> Int
index r = case r of
    Rax -> 0
    Rcx -> 1
    Rdx -> 2
    Rbx -> 3
    Rsp -> 4
    Rbp -> 5
    Rsi -> 6
    Rdi -> 7
