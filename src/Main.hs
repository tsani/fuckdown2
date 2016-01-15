{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Asm as A
import Assembler
import Compiler
import Memory
import Parser

import Data.Attoparsec.ByteString ( parseOnly )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C8
import Foreign
import System.Exit ( exitFailure )

main :: IO ()
main = do
    stdin <- C8.getContents
    bf <- case parseOnly brainfuck stdin of
        Left e -> do
            putStrLn "Parse error: "
            putStrLn e
            exitFailure
        Right x -> return x

    let asm = asmFunction $ do
            mov rax rdi
            compileFuck bf

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
