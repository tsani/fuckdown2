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
import System.Exit ( exitSuccess, exitFailure )

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

exampleAsm :: AsmF addr ()
exampleAsm = do
    l <- label
    A.loop (A l)

main :: IO ()
main = do
    -- let (Right bs) = assemble exampleAsm
    -- print (BS.unpack bs)
    -- _ <- exitSuccess

    let asm = asmFunction $ do
            -- int (I 3)
            mov rax rdi
            compileFuck example
    TextIO.putStr (PA.pretty asm)
    code <- case assemble asm of
        Left e -> do
            putStr "Assembler error: "
            case e of
                InvalidOpcode -> putStrLn "invalid opcode"
                UnsupportedOpcode _ -> do
                    putStrLn "unsupported opcode"
            exitFailure
        Right bs -> do
            putStrLn "Generated code: "
            putStr (show (BS.unpack bs))
            return bs
    let b = BS.toStrict code
    print $ BSS.length b
    -- Convert the bytestring to a function
    f <- byteStringFunction b
    -- Allocate a chunk of memory to use, pass it to the function, and run it
    () <- allocaBytes 4096 f
    putStr "Done."
