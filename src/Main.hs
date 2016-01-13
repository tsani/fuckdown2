{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Asm as A
import Assembler
import Compiler
import DSL
import Free
import Fuckdown as F
import qualified Interpreter as I
import Memory
import qualified PrettyAsm as PA
import Subtype

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import qualified Data.Text.Lazy.IO as TextIO
import Foreign
import System.Exit ( exitFailure )

exec :: Free FuckDSL r -> I.Interpreter r
exec = foldFM I.interpret

example :: ( Functor f
           , Inc :<: f
           , Dec :<: f
           , GoLeft :<: f
           , GoRight :<: f
           , Output :<: f
           , Loop f :<: f
           )
        => Free f ()
example = do
    mkString "hi!\n"
    F.loop $ do
        output
        right

exampleAsm :: AsmF label addr ()
exampleAsm = do
    l <- label
    A.loop (L l)

main :: IO ()
main = do
    let asm = asmFunction $ do
            mov rax rdi
            compileFuck example

    code <- case assemble asm of
        Left e -> do
            putStr "Assembler error: "
            case e of
                InvalidOpcode -> putStrLn "invalid opcode"
                UnsupportedOpcode _ -> putStrLn "unsupported opcode"
                UnassignedLabel -> putStrLn "unassigned label"
                LabelError _ -> putStrLn "label lookup failed"
            exitFailure
        Right bs -> do
            return bs

    let b = BS.toStrict code
    f <- byteStringFunction b
    mem <- callocBytes 4096
    () <- f mem
    free mem
