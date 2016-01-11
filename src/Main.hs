{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Asm hiding (loop)
import Assembler
import Compiler
import DSL
import Free
import Fuckdown
import qualified Interpreter as I
import qualified Pretty as PF
import qualified PrettyAsm as PA
import Subtype

import Control.Monad.State
import qualified Data.Text.Lazy.IO as T

exec :: Free FuckDSL r -> I.Interpreter r
exec = foldFM I.interpret

example :: (Functor f, Inc :<: f, GoLeft :<: f, GoRight :<: f, Output :<: f, Loop f :<: f) => Free f ()
example = do
    mkString "Hello, world!\n"
    loop $ do
        output
        right

main = do
    putStrLn (PF.pretty_ example)
    runStateT (I.runInterpreter (exec example)) I.initialState
    let asm = compileFuck example
    T.putStr (PA.pretty asm)
