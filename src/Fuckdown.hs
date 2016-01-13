{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fuckdown where

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

-- | Just a flipped version of "replicateM_".
basicMany :: Monad m => m a -> Int -> m ()
basicMany a = flip replicateM_ a

-- | Increment the cell under the cursor a given number of times, effectively
-- increasing its value by the given "Word8".
incMany :: (Functor f, Inc :<: f) => Word8 -> Free f ()
incMany = basicMany inc . fromIntegral

-- | Shift the cursor to the left a given number of times.
leftMany :: (Functor f, GoLeft :<: f) => Int -> Free f ()
leftMany = basicMany left

-- | Shift the cursor to the right a given number of times.
rightMany :: (Functor f, GoRight :<: f) => Int -> Free f ()
rightMany = basicMany right

-- | Zeroes out the cell under the cursor.
zeroCell :: (Functor f, Dec :<: f, Loop f :<: f) => Free f ()
zeroCell = loop dec

-- | Write a given character to the cell under the cursor.
--
-- /Warning/: this function assumes that the value under the cursor is zero.
mkChar :: (Functor f, Inc :<: f) => Char -> Free f ()
mkChar = incMany . fromIntegral . fromEnum

-- | Write a string to sequential rightward cells under the cursor.
--
-- /Warning/: this function assumes that the value of each cell that would be
-- occupied by the string is zero.
mkString :: (Functor f, Inc :<: f, GoLeft :<: f, GoRight :<: f, Loop f :<: f, Dec :<: f)
         => String -> Free f ()
mkString [] = zeroCell
mkString (c:cs) = do
    mkChar c
    right
    mkString cs
    left
