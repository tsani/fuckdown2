{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Free where

import Control.Monad ( ap )

import Subtype

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
    -- fmap :: (a -> b) -> (Free f a -> Free f b)
    fmap f (Pure x) = Pure (f x) -- since x :: a
    fmap f (Free fa) = Free ((fmap . fmap) f fa)

instance (Functor f, f :<: g) => Free f :<: Free g where
    inject (Pure x) = Pure x
    inject (Free f) = Free $ inject $ fmap inject f

instance Functor f => Applicative (Free f) where
    pure = Pure
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return = Pure
    (Pure x) >>= k = k x
    (Free f) >>= k = Free (fmap (>>= k) f)

-- | Promotes a functor into its associated free monad.
liftF :: Functor f => f r -> Free f r
liftF f = Free (fmap Pure f)

foldF :: Functor f => (f r -> r) -> Free f r -> r
foldF _       (Pure x) = x
foldF extract (Free f) = extract (fmap (foldF extract) f)

-- | Tear down a free monad monadically, reinterpreting it in another monad.
foldFM :: (Monad m, Functor f) => (f (m r) -> m r) -> Free f r -> m r
foldFM _       (Pure x) = return x
foldFM extract (Free f) = extract (fmap (foldFM extract) f)
