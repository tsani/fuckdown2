{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Asm as A
import Assembler
import Compiler
import Memory
import Parser
import Pretty ( pretty_ )
import PrettyAsm ( pretty )

import Data.Attoparsec.ByteString ( parseOnly )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Lazy.IO as TIO
import Foreign
import System.Environment ( getArgs )
import System.Exit ( exitFailure )

main :: IO ()
main = do
    arg1:_ <- getArgs
    file <- C8.readFile arg1

    bf <- case parseOnly brainfuck file of
        Left e -> do
            putStrLn "Parse error: "
            putStrLn e
            exitFailure
        Right x -> return x

    let prettyBf = pretty_ bf

    writeFile "out.bf" prettyBf

    let asm = asmFunction $ do
            mov rax rdi
            compileFuck bf

    TIO.writeFile "out.asm" (pretty asm)

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
    putStrLn $ "Assembled " ++ show (C8.length b) ++ " bytes."
    f <- byteStringFunction b
    mem <- callocBytes 4096
    () <- f mem
    free mem
