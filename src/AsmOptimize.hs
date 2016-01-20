module AsmOptimize where

import Asm
import Free

optimizeAsm :: (Eq label, Eq addr) => AsmF label addr a -> AsmF label addr a
optimizeAsm (Free (Inc v next)) =
    let (count, next') = go 1 next

        go n (Free (Inc v' next''))
            | v == v' = go (n + 1) next''
            | otherwise = (n, Free (Inc v' next''))
        go n f = (n, f)

        in if count == 1
            then Free (Inc v (optimizeAsm next))
            else Free (Add v (I count) (optimizeAsm next'))

optimizeAsm (Free (Dec v next)) =
    let (count, next') = go 1 next

        go n (Free (Dec v' next''))
            | v == v' = go (n + 1) next''
            | otherwise = (n, Free (Dec v' next''))
        go n f = (n, f)

    in if count == 1
        then Free (Dec v (optimizeAsm next))
        else Free (Sub v (I count) (optimizeAsm next'))

optimizeAsm (Free f) = Free $ fmap optimizeAsm f
optimizeAsm (Pure x) = Pure x
