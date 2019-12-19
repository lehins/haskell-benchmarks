{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


randomArrayWord64 ::
     WorkerStates g -> Sz1 -> (g -> IO Word64) -> IO (Array P Ix1 Word64)
randomArrayWord64 = randomArrayWS
{-# INLINE randomArrayWord64 #-}

randomArrayDouble ::
     WorkerStates g -> Sz1 -> (g -> IO Double) -> IO (Array P Ix1 Double)
randomArrayDouble = randomArrayWS
{-# INLINE randomArrayDouble #-}
