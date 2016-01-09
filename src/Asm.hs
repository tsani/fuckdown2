module Asm where

import Free

data Val
    = I Int64
    | R Reg
    | A Word32
    deriving (Eq, Show)

data Reg
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rbp
    | Rsi
    | Rdi
    deriving (Eq, Show)

index :: Reg -> Int
index r = case r of
    Rax -> 0
    Rcx -> 1
    Rdx -> 2
    Rbx -> 3
    Rsp -> 4
    Rbp -> 5
    Rsi -> 6
    Rdi -> 7

data InstructionF f
    = Ret f
    | Mov Val Val f
    | Add Val Val f
    | Sub Val Val f
    | Mul Val f
    | IMult Val f
    | Xor Val Val f
    | Inc Val f
    | Dec Val f
    | Push Val f
    | Pop Val f
    | Call Val f
    | Loop Val f
    | Nop f
    | Syscall f
    deriving (Eq, Show)

type Instruction = Free Instruction

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
