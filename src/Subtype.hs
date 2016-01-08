{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Subtype where

class sub :<: sup where
    inject :: sub a -> sup a

instance f :<: f where
    inject = id
