{-# LANGUAGE DeriveFunctor #-}

module Asm where

import Data.Int
import Data.Word

import Free

type Immediate = Int64
type Address = Word64

data Val label addr
    = I Immediate
    | R Register
    | IR Register
    | A addr
    | L label
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
data Asm label addr f
    = Ret f
    | Mov (Val label addr) (Val label addr) f
    | Add (Val label addr) (Val label addr) f
    | Sub (Val label addr) (Val label addr) f
    | Mul (Val label addr) f
    | IMul (Val label addr) f
    | Xor (Val label addr) (Val label addr) f
    | Inc (Val label addr) f
    | Dec (Val label addr) f
    | Push (Val label addr) f
    | Pop (Val label addr) f
    | Jmp (Val label addr) f
    | Loop (Val label addr) f
    | Nop f
    | Syscall f
    | NewLabel (label -> f)
    | SetLabel label f
    | Here (addr -> f)
    | Int (Val label addr) f
    | Cmp (Val label addr) (Val label addr) f
    | Je (Val label addr) f
    | Jne (Val label addr) f
    deriving (Functor)

type AsmF label addr = Free (Asm label addr)

-- Shorthands for Free-wrapped assembly commands.

ret :: AsmF label addr ()
ret = liftF . Ret $ ()

mov :: Val label addr -> Val label addr -> AsmF label addr ()
mov v1 v2 = liftF . Mov v1 v2 $ ()

add :: Val label addr -> Val label addr -> AsmF label addr ()
add v1 v2 = liftF . Add v1 v2 $ ()

sub :: Val label addr -> Val label addr -> AsmF label addr ()
sub v1 v2 = liftF . Sub v1 v2 $ ()

mul :: Val label addr -> AsmF label addr ()
mul v1 = liftF . Mul v1 $ ()

imul :: Val label addr -> AsmF label addr ()
imul v1 = liftF . IMul v1 $ ()

xor :: Val label addr -> Val label addr -> AsmF label addr ()
xor v1 v2 = liftF . Xor v1 v2 $ ()

inc :: Val label addr -> AsmF label addr ()
inc v1 = liftF . Inc v1 $ ()

dec :: Val label addr -> AsmF label addr ()
dec v1 = liftF . Dec v1 $ ()

push :: Val label addr -> AsmF label addr ()
push v1 = liftF . Push v1 $ ()

pop :: Val label addr -> AsmF label addr ()
pop v1 = liftF . Pop v1 $ ()

jmp :: Val label addr -> AsmF label addr ()
jmp v1 = liftF . Jmp v1 $ ()

loop :: Val label addr -> AsmF label addr ()
loop v1 = liftF . Loop v1 $ ()

nop :: AsmF label addr ()
nop = liftF . Nop $ ()

syscall :: AsmF label addr ()
syscall = liftF . Syscall $ ()

newLabel :: AsmF label addr label
newLabel = liftF . NewLabel $ id

setLabel :: label -> AsmF label addr ()
setLabel l = liftF . SetLabel l $ ()

here :: AsmF label addr addr
here = liftF . Here $ id

label :: AsmF label addr label
label = do
    l <- newLabel
    setLabel l
    return l

int :: Val label addr -> AsmF label addr ()
int v1 = liftF . Int v1 $ ()

cmp :: Val label addr -> Val label addr -> AsmF label addr ()
cmp v1 v2 = liftF . Cmp v1 v2 $ ()

je :: Val label addr -> AsmF label addr ()
je v1 = liftF . Je v1 $ ()

jne :: Val label addr -> AsmF label addr ()
jne v1 = liftF . Jne v1 $ ()

jz :: Val label addr -> AsmF label addr ()
jz = je

jnz :: Val label addr -> AsmF label addr ()
jnz = jne

-- Shorthands for registers

rax :: Val label addr
rax = R Rax

rbx :: Val label addr
rbx = R Rbx

rcx :: Val label addr
rcx = R Rcx

rdx :: Val label addr
rdx = R Rdx

rbp :: Val label addr
rbp = R Rbp

rsi :: Val label addr
rsi = R Rsi

rdi :: Val label addr
rdi = R Rdi

rsp :: Val label addr
rsp = R Rsp

-- Shorthands for indirect register addressing

irax :: Val label addr
irax = IR Rax

irbx :: Val label addr
irbx = IR Rbx

ircx :: Val label addr
ircx = IR Rcx

irdx :: Val label addr
irdx = IR Rdx

irbp :: Val label addr
irbp = IR Rbp

irsp :: Val label addr
irsp = IR Rsp

irsi :: Val label addr
irsi = IR Rsi

irdi :: Val label addr
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
asmFunction :: AsmF label addr () -> AsmF label addr ()
asmFunction body = do
    push rbp
    mov rbp rsp
    body
    mov rsp rbp
    pop rbp
    ret
