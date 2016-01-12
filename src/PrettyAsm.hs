{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyAsm where

import Asm
import Free

import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid ( (<>) )
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

type AsmPretty = Asm Builder
type AsmPrettyF = Free AsmPretty

data PrettyAsmState
    = PrettyAsmState
        { lastLabelId :: Int
        }

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

initialState = PrettyAsmState { lastLabelId = 0 }

nextLabel :: PrettyAsm Builder
nextLabel = do
    i <- gets lastLabelId
    modify $ \s -> s { lastLabelId = i + 1 }
    return ("l" <> decimal i)

-- | Represent a value as text.
val :: Val Builder -> Builder
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

line m t = (<>) <$> pure t <*> m

prettyArg :: AsmPretty (PrettyAsm Builder) -> PrettyAsm Builder
prettyArg a = case a of
    Ret m ->
        line m $ "ret\n"
    Mov v1 v2 m ->
        line m $ "mov " <> val v1 <> ", " <> val v2 <> "\n"
    Add v1 v2 m -> do
        line m $ "add " <> val v1 <> ", " <> val v2 <> "\n"
    Sub v1 v2 m -> do
        line m $ "sub " <> val v1 <> ", " <> val v2 <> "\n"
    Mul v1 m -> do
        line m $ "mul " <> val v1 <> "\n"
    IMul v1 m -> do
        line m $ "imul " <> val v1 <> "\n"
    Xor v1 v2 m -> do
        line m $ "xor " <> val v1 <> ", " <> val v2 <> "\n"
    Inc v1 m -> do
        line m $ "inc " <> val v1 <> "\n"
    Dec v1 m -> do
        line m $ "dec " <> val v1 <> "\n"
    Push v1 m -> do
        line m $ "push " <> val v1 <> "\n"
    Pop v1 m -> do
        line m $ "pop " <> val v1 <> "\n"
    Jmp v1 m -> do
        line m $ "jmp " <> val v1 <> "\n"
    Loop v1 m -> do
        line m $ "loop " <> val v1 <> "\n"
    Nop m -> do
        line m $ "nop\n"
    Syscall m -> do
        line m $ "syscall\n"
    Label k -> do
        l <- nextLabel
        let m = k l
        line m $ l <> ":\n"

pretty :: AsmPrettyF a -> T.Text
pretty asm
    = toLazyText
    $ runIdentity
    $ flip evalStateT initialState
    $ runPrettyAsmT
    $ foldFM prettyArg
    $ asm $> ""
