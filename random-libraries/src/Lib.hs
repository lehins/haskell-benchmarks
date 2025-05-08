-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Tuple
import Control.Monad.Random.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import System.Random.Stateful


randomWithRejectionN ::
     (Random a, RandomGen g, Monad m) => Int -> (a -> m Bool) -> g -> m ([a], g)
randomWithRejectionN n f g = fmap swap $ runWriterT $ flip execRandT g $ evalStateT go 0
  where
    go = do
      i <- get
      when (i < n) $ do
        v <- lift getRandom
        reject <- lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go



randomWithRejectionNs ::
     (Random a, RandomGen g, Ord s, Num s, Monad m)
  => s
  -> (a -> m Bool)
  -> g
  -> m ([a], g)
randomWithRejectionNs n f g =
  fmap (fmap unStateGen . swap) $
  runWriterT $ flip execStateT (StateGen g) $ evalStateT go 0
  where
    go = do
      i <- get
      when (i < n) $ do
        v <- lift $ randomM StateGenM
        reject <- lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go


evalStateGenT ::
     (Functor m, RandomGen g) => g -> (StateGenM g -> StateT g m a) -> m a
evalStateGenT g action = fst <$> runStateGenT g action

execStateGenT ::
     (Functor m, RandomGen g) => g -> (StateGenM g -> StateT g m a) -> m g
execStateGenT g action = snd <$> runStateGenT g action



askUniform :: (Uniform a, StatefulGen g m, MonadReader g m) => m a
askUniform = ask >>= uniformM


askRandom :: (Random a, RandomGenM g r m, MonadReader g m) => m a
askRandom = ask >>= randomM

randomWithRejectionNr ::
     (Random a, RandomGen g, Monad m) => Int -> (a -> m Bool) -> g -> m ([a], g)
randomWithRejectionNr n f g =
  fmap swap $ runWriterT $ execStateGenT g $ runReaderT $ evalStateT go 0
  where
    go = do
      i <- get
      when (i < n) $ do
        v <- lift $ askRandom
        reject <- lift $ lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go


randomWithRejectionN' ::
     (Random a, RandomGen g, Ord s, Num s, Monad m)
  => s
  -> (a -> m Bool)
  -> g
  -> m ([a], g)
randomWithRejectionN' n f g =
  fmap swap $ runWriterT $ execStateGenT g $ \sg -> evalStateT (go sg) 0
  where
    go sg = do
      i <- get
      when (i < n) $ do
        v <- lift $ randomM sg
        reject <- lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go sg

data RandGenM g = RandGenM

instance (Monad m, RandomGen g) => StatefulGen (RandGenM g) (RandT g m) where
  uniformWord32R r = applyRand (genWord32R r)
  uniformWord64R r = applyRand (genWord64R r)
  uniformWord8 = applyRand genWord8
  uniformWord16 = applyRand genWord16
  uniformWord32 = applyRand genWord32
  uniformWord64 = applyRand genWord64
  uniformShortByteString n = applyRand (genShortByteString n)

instance (Monad m, RandomGen g) => RandomGenM (RandGenM g) g (RandT g m) where
  applyRandomGenM = applyRand

applyRand :: Applicative m => (g -> (a, g)) -> RandGenM g -> RandT g m a
applyRand f _ = liftRandT (pure . f)


uniformWithRejectionN ::
     forall a g m. (Uniform a, RandomGen g, Monad m)
  => Int
  -> (a -> m Bool)
  -> g
  -> m ([a], g)
uniformWithRejectionN n f g =
  fmap swap $ runWriterT $ flip execRandT g $ evalStateT go 0
  where
    go = do
      i <- get
      when (i < n) $ do
        v <- lift $ uniformM (RandGenM :: RandGenM g)
        reject <- lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go


randomWithRejectionN2 ::
     forall a g m. (Random a, RandomGen g, Monad m)
  => Int -> (a -> m Bool) -> g -> m ([a], g)
randomWithRejectionN2 n f g =
  fmap swap $ runWriterT $ flip execRandT g $ evalStateT go 0
  where
    go = do
      i <- get
      when (i < n) $ do
        v <- lift $ randomM (RandGenM :: RandGenM g)
        reject <- lift $ lift $ lift $ f v
        unless reject $ do
          lift (lift (tell [v]))
          put (i + 1)
        go

uniformWithRejectionN' ::
     (Uniform a, StatefulGen g m) => Int -> (a -> m Bool) -> g -> m [a]
uniformWithRejectionN' n f g = replicateM n go
  where
    go = do
      x <- uniformM g
      reject <- f x
      if reject
        then go
        else pure x
