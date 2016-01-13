module Zipper where

data Zipper a
    = Zipper 
        { left :: [a]
        , focus :: a
        , right :: [a]
        }

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

-- | Shifts the focus of the zipper to the left by one cell.
shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zipper _ _ []) = error "Zipper cannot shift left"
shiftLeft (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

-- | Shifts the focus of the zipper to the right by one cell.
shiftRight :: Zipper a -> Zipper a
shiftRight (Zipper [] _ _) = error "Zipper cannot shift right"
shiftRight (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

focussing :: (a -> a) -> Zipper a -> Zipper a
focussing f (Zipper ls x rs) = Zipper ls (f x) rs
