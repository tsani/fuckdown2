{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import Coproduct
import DSL
import Free

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

parseLeft :: (Functor f, GoLeft :<: f) => Parser (Free f ())
parseLeft = do
    _ <- char '<'
    return left

parseRight :: (Functor f, GoRight :<: f) => Parser (Free f ())
parseRight = do
    _ <- char '>'
    return right

parseInc :: (Functor f, Inc :<: f) => Parser (Free f ())
parseInc = do
    _ <- char '+'
    return inc

parseDec :: (Functor f, Dec :<: f) => Parser (Free f ())
parseDec = do
    _ <- char '-'
    return dec

parseOutput :: (Functor f, Output :<: f) => Parser (Free f ())
parseOutput = do
    _ <- char '.'
    return output

parseInput :: (Functor f, Input :<: f) => Parser (Free f ())
parseInput = do
    _ <- char ','
    return input

parseLoop :: ( Functor f
             , GoLeft :<: f
             , GoRight :<: f
             , Inc :<: f
             , Dec :<: f
             , Input :<: f
             , Output :<: f
             , Loop f :<: f
             )
          => Parser (Free f ())
parseLoop = do
    _ <- char '['
    body <- many command
    _ <- char ']'
    return (loop (sequence_ body))

command :: 
    ( Functor f
    , GoLeft :<: f
    , GoRight :<: f
    , Inc :<: f
    , Dec :<: f
    , Input :<: f
    , Output :<: f
    , Loop f :<: f
    )
    => Parser (Free f ())
command
    = parseLeft
    <|> parseRight
    <|> parseInc
    <|> parseDec
    <|> parseOutput
    <|> parseInput
    <|> parseLoop

brainfuck ::
    ( Functor f
    , GoLeft :<: f
    , GoRight :<: f
    , Inc :<: f
    , Dec :<: f
    , Input :<: f
    , Output :<: f
    , Loop f :<: f
    )
    => Parser (Free f ())
brainfuck
    = sequence_ <$> many command
