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
import Memory
import qualified Pretty as PF
import qualified PrettyAsm as PA
import Subtype

import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Unsafe ( unsafeUseAsCString )
import qualified Data.Text.Lazy.Builder as TextB
import Data.Text.Lazy.Builder.Int ( hexadecimal )
import qualified Data.Text.Lazy.IO as TextIO
import Foreign
import System.Exit ( exitFailure )

exec :: Free FuckDSL r -> I.Interpreter r
exec = foldFM I.interpret

example :: (Functor f, Inc :<: f, GoLeft :<: f, GoRight :<: f, Output :<: f, Loop f :<: f) => Free f ()
example = do
    mkString "Hello, world!\n"
    loop $ do
        output
        right

main = do
    -- putStrLn (PF.pretty_ example)
    -- runStateT (I.runInterpreter (exec example)) I.initialState
    let asm = asmFunction $ do
            mov rcx rdi
            compileFuck example
    TextIO.putStr (PA.pretty asm)
    code <- case assemble asm of
        Left e -> do
            putStr "Assembler error: "
            case e of
                InvalidOpcode -> putStrLn "invalid opcode"
                UnsupportedOpcode asm -> do
                    putStrLn "unsupported opcode"
            exitFailure
        Right bs -> do
            putStrLn "Generated code: "
            putStr (show (BS.unpack bs))
            return bs
    f <- byteStringFunction (BS.toStrict code)
    () <- allocaBytes 4096 f
    putStr "Done."
