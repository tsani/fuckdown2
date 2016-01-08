{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module DSL where

import Coproduct
import Fuckdown

-- | The Brainfuck DSL as a functor coproduct of the different actions
-- available in Brainfuck.
newtype FuckDSL a
    = FuckDSL
        { getDSL :: 
            ( GoLeft 
            :+: GoRight 
            :+: Inc 
            :+: Dec 
            :+: Input 
            :+: Output 
            :+: Loop FuckDSL
            ) a
        }
    deriving (Functor)

instance Loop FuckDSL :<: FuckDSL where
    inject = FuckDSL . inject

instance Output :<: FuckDSL where
    inject = FuckDSL . InL . InR

instance Input :<: FuckDSL where
    inject = FuckDSL . InL . InL . InR

instance Dec :<: FuckDSL where
    inject = FuckDSL . InL . InL . InL . InR

instance Inc :<: FuckDSL where
    inject = FuckDSL . InL . InL . InL . InL . InR

instance GoRight :<: FuckDSL where
    inject = FuckDSL . InL . InL . InL . InL . InL . InR

instance GoLeft :<: FuckDSL where
    inject = FuckDSL . InL . InL . InL . InL . InL . InL
