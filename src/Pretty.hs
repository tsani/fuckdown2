{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Pretty where

import Coproduct
import DSL
import Free
import Fuckdown

class Functor f => Pretty f where
    prettyArg :: f String -> String

instance (Pretty f, Pretty g) => Pretty (f :+: g) where
    prettyArg (InL f) = prettyArg f
    prettyArg (InR g) = prettyArg g

instance Pretty GoLeft where
    prettyArg (GoLeft s) = '<' : s

instance Pretty GoRight where
    prettyArg (GoRight s) = '>' : s

instance Pretty Inc where
    prettyArg (Inc s) = '+' : s

instance Pretty Dec where
    prettyArg (Dec s) = '-' : s

instance Pretty Output where
    prettyArg (Output s) = '.' : s

instance Pretty Input where
    prettyArg (Input s) = ',' : s

instance Pretty f => Pretty (Free f) where
    prettyArg = foldF prettyArg

instance Pretty f => Pretty (Loop f) where
    prettyArg (Loop f s) = '[' : prettyArg f ++ ']' : s

instance Pretty FuckDSL where
    prettyArg (FuckDSL f) = prettyArg f

pretty :: (a -> String) -> Free FuckDSL a -> String
pretty f = prettyArg . fmap f

pretty_ :: Free FuckDSL a -> String
pretty_ = pretty (const "")
