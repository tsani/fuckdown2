{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fuckdown where

import Control.Monad ( ap )

data Free f r = Free (f (Free f r)) | Pure r

data (:+:) f g e
    = InL (f e)
    | InR (g e)
    deriving (Functor, Show)

instance Functor f => Functor (Free f) where
    -- fmap :: (a -> b) -> (Free f a -> Free f b)
    fmap f (Pure x) = Pure (f x) -- since x :: a
    fmap f (Free fa) = Free ((fmap . fmap) f fa)

instance Functor f => Applicative (Free f) where
    pure = Pure
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return = Pure

    -- m :: Free f a
    -- k :: Free f a -> (a -> Free f b) -> Free f b
    (Pure x) >>= k = k x
    (Free f) >>= k = Free (fmap (>>= k) f)

class sub :<: sup where
    inject :: sub a -> sup a

instance f :<: f where
    inject = id

instance f :<: (f :+: g)  where
    inject = InL

instance {-# OVERLAPPABLE #-} (f :<: h) => f :<: (g :+: h) where
    inject = InR . inject

foldF :: Functor f => (f r -> r) -> Free f r -> r
foldF _       (Pure x) = x
foldF extract (Free f) = extract (fmap (foldF extract) f)

foldFM :: (Monad m, Functor f) => (f (m r) -> m r) -> Free f r -> m r
foldFM _       (Pure x) = return x
foldFM extract (Free f) = extract (fmap (foldFM extract) f)

data GoLeft next = GoLeft next deriving (Functor)
data GoRight next = GoRight next deriving (Functor)
data Inc next = Inc next deriving (Functor)
data Dec next = Dec next deriving (Functor)
data Input next = Input next deriving (Functor)
data Output next = Output Char next deriving (Functor)
data Loop f next = Loop (f next) next deriving (Functor)

newtype FuckDSL a = FuckDSL { getDSL :: (GoLeft :+: GoRight :+: Inc :+: Dec :+: Input :+: Output :+: Loop FuckDSL) a }

type FuckDSLF = Free FuckDSL

liftF :: Functor f => f r -> Free f r
liftF f = Free (fmap Pure f)

left :: (Functor f, GoLeft :<: f) => Free f ()
left = liftF . inject . GoLeft $ ()

right :: (Functor f, GoRight :<: f) => Free f ()
right = liftF . inject . GoRight $ ()

inc :: (Functor f, Inc :<: f) => Free f ()
inc = liftF . inject . Inc $ ()

dec :: (Functor f, Dec :<: f) => Free f ()
dec = liftF . inject . Dec $ ()

input :: (Functor f, Input :<: f) => Free f ()
input = liftF . inject . Input $ ()

output :: (Functor f, Output :<: f) => Char -> Free f ()
output c = liftF . inject . Output c $ ()

loop :: (Functor f, Loop f :<: f) => f () -> Free f ()
loop p = liftF . inject . Loop p ()
