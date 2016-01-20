{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyAsm where

import Asm
import Free

import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid ( (<>) )
import Data.String ( IsString )
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

type AsmPretty = Asm Builder Builder
type AsmPrettyF = Free AsmPretty

data PrettyAsmState
    = PrettyAsmState
        { lastLabelId :: Int
        }

-- | The assembly pretty-printer monad transformer. This is simply a state
-- transformer over "PrettyAsmState", such that labels are textual values
-- constructed by using the character 'l' followed by increasing numbers.
newtype PrettyAsmT m a
    = PrettyAsmT
        { runPrettyAsmT :: StateT PrettyAsmState m a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState PrettyAsmState
        )

type PrettyAsm = PrettyAsmT Identity

instance MonadTrans PrettyAsmT where
    lift = PrettyAsmT . lift

-- | A simple initial assembly pretty-printer state, whose last label id is
-- zero.
initialState :: PrettyAsmState
initialState = PrettyAsmState { lastLabelId = 0 }

nextLabel :: PrettyAsm Builder
nextLabel = do
    i <- gets lastLabelId
    modify $ \s -> s { lastLabelId = i + 1 }
    return ("l" <> decimal i)

-- | Represent a value as text.
val :: Val Builder Builder -> Builder
val (I i) = hexadecimal i
val (R r) = case r of
    Rax -> "rax"
    Rbx -> "rbx"
    Rcx -> "rcx"
    Rdx -> "rdx"
    Rbp -> "rbp"
    Rsi -> "rsi"
    Rdi -> "rdi"
    Rsp -> "rsp"
val (IR r) = "[" <> val (R r) <> "]"
val (A b) = b
val (L l) = l

line :: (Applicative f, Monoid b, IsString b) => f b -> b -> f b
line m t = prepend m (t <> "\n")

-- | Prepends some output.
prepend :: (Applicative f, Monoid b) => f b -> b -> f b
prepend m t = (<>) <$> pure t <*> m

prettyArg :: AsmPretty (PrettyAsm Builder) -> PrettyAsm Builder
prettyArg a = case a of
    Ret m -> line m $ "ret"
    Mov v1 v2 m  -> line m $ "mov "  <> val v1 <> ", " <> val v2
    Add v1 v2 m  -> line m $ "add "  <> val v1 <> ", " <> val v2
    Sub v1 v2 m  -> line m $ "sub "  <> val v1 <> ", " <> val v2
    Mul v1 m     -> line m $ "mul "  <> val v1
    IMul v1 m    -> line m $ "imul " <> val v1
    Xor v1 v2 m  -> line m $ "xor "  <> val v1 <> ", " <> val v2
    Inc v1 m     -> line m $ "inc "  <> val v1
    Dec v1 m     -> line m $ "dec "  <> val v1
    Push v1 m    -> line m $ "push " <> val v1
    Pop v1 m     -> line m $ "pop "  <> val v1
    Jmp v1 m     -> line m $ "jmp "  <> val v1
    Loop v1 m    -> line m $ "loop " <> val v1
    Int v1 m     -> line m $ "inc "  <> val v1
    Cmp v1 v2 m  -> line m $ "cmp "  <> val v1 <> ", " <> val v2
    Je v1 m      -> line m $ "je "   <> val v1
    Jne v1 m     -> line m $ "jne "  <> val v1
    Nop m        -> line m $ "nop"
    Syscall m    -> line m $ "syscall"
    NewLabel k -> do
        l <- nextLabel
        k l
    SetLabel l m -> line m (l <> ":")
    Here k -> k "$"

pretty :: AsmPrettyF a -> T.Text
pretty asm
    = toLazyText
    $ runIdentity
    $ flip evalStateT initialState
    $ runPrettyAsmT
    $ foldFM prettyArg
    $ asm $> ""
