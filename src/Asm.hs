{-# LANGUAGE DeriveFunctor #-}

module Asm where

import Data.Int
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
    | Int (Val addr) f
    | Cmp (Val addr) (Val addr) f
    | Je (Val addr) f
    | Jne (Val addr) f
    deriving (Functor)

type AsmF addr = Free (Asm addr)

-- Shorthands for Free-wrapped assembly commands.

ret :: AsmF addr ()
ret = liftF . Ret $ ()

mov :: Val addr -> Val addr -> AsmF addr ()
mov v1 v2 = liftF . Mov v1 v2 $ ()

add :: Val addr -> Val addr -> AsmF addr ()
add v1 v2 = liftF . Add v1 v2 $ ()

sub :: Val addr -> Val addr -> AsmF addr ()
sub v1 v2 = liftF . Sub v1 v2 $ ()

mul :: Val addr -> AsmF addr ()
mul v1 = liftF . Mul v1 $ ()

imul :: Val addr -> AsmF addr ()
imul v1 = liftF . IMul v1 $ ()

xor :: Val addr -> Val addr -> AsmF addr ()
xor v1 v2 = liftF . Xor v1 v2 $ ()

inc :: Val addr -> AsmF addr ()
inc v1 = liftF . Inc v1 $ ()

dec :: Val addr -> AsmF addr ()
dec v1 = liftF . Dec v1 $ ()

push :: Val addr -> AsmF addr ()
push v1 = liftF . Push v1 $ ()

pop :: Val addr -> AsmF addr ()
pop v1 = liftF . Pop v1 $ ()

jmp :: Val addr -> AsmF addr ()
jmp v1 = liftF . Jmp v1 $ ()

loop :: Val addr -> AsmF addr ()
loop v1 = liftF . Loop v1 $ ()

nop :: AsmF addr ()
nop = liftF . Nop $ ()

syscall :: AsmF addr ()
syscall = liftF . Syscall $ ()

label :: AsmF addr addr
label = liftF . Label $ id

int :: Val addr -> AsmF addr ()
int v1 = liftF . Int v1 $ ()

cmp :: Val addr -> Val addr -> AsmF addr ()
cmp v1 v2 = liftF . Cmp v1 v2 $ ()

je :: Val addr -> AsmF addr ()
je v1 = liftF . Je v1 $ ()

jne :: Val addr -> AsmF addr ()
jne v1 = liftF . Jne v1 $ ()

jz :: Val addr -> AsmF addr ()
jz = je

jnz :: Val addr -> AsmF addr ()
jnz = jne

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
