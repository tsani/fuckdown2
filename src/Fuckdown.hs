{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fuckdown where

import Control.Monad ( ap, when, replicateM_ )
import Control.Monad.State
import Data.Word

import Coproduct
import Free

data GoLeft next = GoLeft next deriving (Functor)
data GoRight next = GoRight next deriving (Functor)
data Inc next = Inc next deriving (Functor)
data Dec next = Dec next deriving (Functor)
data Input next = Input next deriving (Functor)
data Output next = Output next deriving (Functor)
data Loop f next = Loop (Free f next) next deriving (Functor)

-- | Moves the data pointer to the left.
left :: (Functor f, GoLeft :<: f) => Free f ()
left = liftF . inject . GoLeft $ ()

-- | Moves the data pointer to the right.
right :: (Functor f, GoRight :<: f) => Free f ()
right = liftF . inject . GoRight $ ()

-- | Increments the cell under the data pointer.
inc :: (Functor f, Inc :<: f) => Free f ()
inc = liftF . inject . Inc $ ()

-- | Decrements the cell under the data pointer.
dec :: (Functor f, Dec :<: f) => Free f ()
dec = liftF . inject . Dec $ ()

-- | Reads an ASCII character from standard input, and records its value in the
-- cell under the data pointer.
input :: (Functor f, Input :<: f) => Free f ()
input = liftF . inject . Input $ ()

-- | Interprets the cell under the data pointer as an ASCII character and
-- writes it to standard output.
output :: (Functor f, Output :<: f) => Free f ()
output = liftF . inject . Output $ ()

-- | Loops while the cell under the data pointer is nonzero.
loop :: (Functor f, Loop f :<: f) => Free f () -> Free f ()
loop p = liftF . inject . Loop p $ ()

basicMany a = flip replicateM_ a

incMany :: (Functor f, Inc :<: f) => Word8 -> Free f ()
incMany = basicMany inc . fromIntegral

leftMany :: (Functor f, GoLeft :<: f) => Int -> Free f ()
leftMany = basicMany left

rightMany :: (Functor f, GoRight :<: f) => Int -> Free f ()
rightMany = basicMany right

mkChar :: (Functor f, Inc :<: f) => Char -> Free f ()
mkChar = incMany . fromIntegral . fromEnum

mkString :: (Functor f, Inc :<: f, GoLeft :<: f, GoRight :<: f) => String -> Free f ()
mkString [] = return ()
mkString (c:cs) = do
    mkChar c
    right
    mkString cs
    left
