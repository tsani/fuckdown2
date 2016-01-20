{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import Control.Monad.State
import Data.Word

import Coproduct
import DSL
import Free
import qualified Zipper as Z

class (Functor f, Monad m) => Interpret f m where
    interpret :: f (m r) -> m r 

instance (Monad m, Interpret f m, Interpret g m) => Interpret (f :+: g) m where
    interpret (InL f) = interpret f
    interpret (InR g) = interpret g

type Memory = Z.Zipper Word8

between :: Monad m => m a -> m b -> m a
between m a = do
    _ <- a
    m

instance MonadState Memory m => Interpret GoLeft m where
    interpret (GoLeft m) = between m $ modify Z.shiftLeft

instance MonadState Memory m => Interpret GoRight m where
    interpret (GoRight m) = between m $ modify Z.shiftRight

instance MonadState Memory m => Interpret Inc m where
    interpret (Inc m) = between m $ modify $ Z.focussing (+1)

instance MonadState Memory m => Interpret Dec m where
    interpret (Dec m) = between m $ modify $ Z.focussing (subtract 1)

instance (MonadIO m, MonadState Memory m) => Interpret Input m where
    interpret (Input m) = between m $ do
        c <- liftIO $ getChar
        modify . Z.focussing . const . fromIntegral . fromEnum $ c

instance (MonadIO m, MonadState Memory m) => Interpret Output m where
    interpret (Output m) = between m $ do
        x <- gets Z.focus
        let c = toEnum . fromIntegral $ x
        liftIO $ putChar c

instance (MonadState Memory m, Interpret f m) => Interpret (Loop f) m where
    interpret (Loop body m) = between m l where
        l = do
            x <- gets Z.focus
            when (x > 0) $ do
                _ <- foldFM interpret body
                l

newtype Interpreter a
    = Interpreter
        { runInterpreter :: StateT Memory IO a }
    deriving ( Functor, Applicative, Monad, MonadState Memory, MonadIO )

initialState :: Z.Zipper Word8
initialState = Z.Zipper (repeat 0) 0 (repeat 0)

instance Interpret FuckDSL Interpreter where
    interpret (FuckDSL f) = interpret f

exec :: Free FuckDSL r -> IO r
exec f = evalStateT (runInterpreter (foldFM interpret f)) initialState
