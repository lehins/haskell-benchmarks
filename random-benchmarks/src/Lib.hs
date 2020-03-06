{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Lib where

import Control.Monad
import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Prelude as P
import System.Random as System
import Data.Word

computeIO :: (Mutable r ix e, Load r' ix e) => Array r' ix e -> IO (Array r ix e)
computeIO arr = loadArray arr >>= unsafeFreeze (getComp arr)
{-# INLINE computeIO #-}

randomArrayPureSeq ::
     forall a g. Prim a
  => g
  -> (g -> (a, g))
  -> Sz1
  -> IO (Array P Ix1 a)
randomArrayPureSeq gen getRandom sz = computeIO $ randomArray gen (\g -> (g, g)) getRandom Seq sz
{-# INLINE randomArrayPureSeq #-}

randomArrayPurePar ::
     forall a g. (RandomGen g, Prim a)
  => g
  -> (g -> (a, g))
  -> Sz1
  -> IO (Array P Ix1 a)
randomArrayPurePar gen getRandom sz =
  computeIO $ randomArray gen split getRandom Par sz
{-# INLINE randomArrayPurePar #-}


randomArrayPrimGen ::
     ( Random e
     , Mutable r ix e
     , PrimMonad m
     , MonadUnliftIO m
     , RandomGen g
     , PrimState m ~ RealWorld
     )
  => g
  -> Comp
  -> Sz ix
  -> m (Array r ix e)
randomArrayPrimGen g comp sz =
  runPrimGenIO_ g $ \primGen -> generateArrayLinear comp sz $ \_ -> randomM primGen
{-# INLINE randomArrayPrimGen #-}

randomArrayPrimGen64 ::
     (PrimMonad m, MonadUnliftIO m, RandomGen g, PrimState m ~ RealWorld)
  => g
  -> Comp
  -> Sz1
  -> m (Array P Ix1 Word64)
randomArrayPrimGen64 = randomArrayPrimGen
{-# INLINE randomArrayPrimGen64 #-}

randomArrayMutGen ::
     ( Random e
     , PrimMonad m
     , Mutable r ix e
     , MonadIO m
     , RandomGen g
     , Prim g
     , PrimState m ~ RealWorld
     )
  => g
  -> Sz ix
  -> m (Array r ix e)
randomArrayMutGen g sz =
  runMutGenIO_ g $ \mutGen -> generateArrayLinearS sz $ \_ -> randomM mutGen
{-# INLINE randomArrayMutGen #-}

randomArrayMutGen64 ::
     (PrimMonad m, MonadIO m, RandomGen g, Prim g, PrimState m ~ RealWorld)
  => g
  -> Sz1
  -> m (Array P Ix1 Word64)
randomArrayMutGen64 = randomArrayMutGen
{-# INLINE randomArrayMutGen64 #-}

randomArrayPureGen ::
     (Random e, RandomGen g, PrimMonad m, Mutable r ix e)
  => g
  -> Sz ix
  -> m (Array r ix e)
randomArrayPureGen g sz =
  runGenStateT_ g $ generateArrayLinearS sz $ \_ -> genRandom
{-# INLINE randomArrayPureGen #-}

randomArrayPureGen64 ::
     (RandomGen g, PrimMonad m) => g -> Sz1 -> m (Array P Ix1 Word64)
randomArrayPureGen64 = randomArrayPureGen
{-# INLINE randomArrayPureGen64 #-}


randomArrayWord64 ::
     WorkerStates g -> Sz1 -> (g -> IO Word64) -> IO (Array P Ix1 Word64)
randomArrayWord64 = randomArrayWS
{-# INLINE randomArrayWord64 #-}

randomArrayDouble ::
     WorkerStates g -> Sz1 -> (g -> IO Double) -> IO (Array P Ix1 Double)
randomArrayDouble = randomArrayWS
{-# INLINE randomArrayDouble #-}
