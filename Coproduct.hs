{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Coproduct 
( module Subtype
, (:+:)(..)
) where

import Subtype

infixl 6 :+:

data (:+:) f g e
    = InL (f e)
    | InR (g e)
    deriving (Functor, Show)

instance f :<: (f :+: g)  where
    inject = InL

instance {-# OVERLAPPABLE #-} (f :<: h) => f :<: (g :+: h) where
    inject = InR . inject
