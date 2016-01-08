{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import DSL
import Free
import Fuckdown
import Interpreter
import Subtype

import Control.Monad.State

exec :: Free FuckDSL r -> Interpreter r
exec = foldFM interpret

example :: (Functor f, Inc :<: f, GoLeft :<: f, GoRight :<: f, Output :<: f, Loop f :<: f) => Free f ()
example = do
    mkString "Hello, world!\n"
    loop $ do
        output
        right

main = runStateT (runInterpreter (exec example)) initialState
