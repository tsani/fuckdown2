{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Asm as A
import AsmOptimize
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


assembleIO asm = case assemble asm of
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

    let optimizedAsm = optimizeAsm asm

    TIO.writeFile "opt.asm" (pretty optimizedAsm)
    TIO.writeFile "out.asm" (pretty asm)

    code <- assembleIO asm
    optimizedCode <- assembleIO optimizedAsm

    let b = BS.toStrict code
    let bOpt = BS.toStrict optimizedCode
    let nb = C8.length b
    let nbOpt = C8.length bOpt
    putStrLn $ "Assembled " ++ show nb ++ " bytes (unoptimized)."
    putStrLn $ "Assembled " ++ show nbOpt ++ " bytes (optimized)."
    putStrLn $ show (fromIntegral (nb - nbOpt) / fromIntegral nb * 100)
        ++ "% improvement"

    f <- byteStringFunction bOpt
    mem <- callocBytes 4096
    () <- f mem
    free mem
