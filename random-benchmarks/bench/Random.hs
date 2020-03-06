{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Scheduler
import Criterion.Main
import Data.Bits
import Data.Int
import Data.List as List
import Data.Massiv.Array
import Data.PCGen
import Data.Word
import Lib
import Random.MWC.Primitive as AC
import Random.MWC.Pure as AC
import Random.Xorshift.Int32
import Random.Xorshift.Int64
import System.Random as Random
import System.Random.Mersenne.Pure64 as PureMT
import System.Random.MWC as MWC
import System.Random.PCG as PCG
import System.Random.PCG.Fast as FastPCG
import System.Random.PCG.Fast.Pure as PureFastPCG
import System.Random.PCG.Pure as PurePCG
import System.Random.Random123
import System.Random.SplitMix as SM
import System.Random.SFMT as SFMT
import System.Random.TF as TF
import qualified System.Random.Mersenne as MT
import qualified System.Random.Xorshift128Plus as Xorshift128Plus
import Text.Printf

instance RandomGen AC.Seed where
  next !g =
    case next_word g of
      (w32, g') -> (fromIntegral w32, g')
  genRange _ = (0, fromIntegral (maxBound :: Word32))
  split _ = error "AC-Random does not support splitting"

instance RandomGen Xorshift128Plus.Gen where
  next !g =
    case Xorshift128Plus.next g of
      (w64, g') -> (fromIntegral w64, g')
  genRange _ = (fromIntegral (minBound :: Int64), fromIntegral (maxBound :: Int64))
  split _ = error "Xorshift128Plus does not support splitting"


randomD :: RandomGen g => g -> (Double, g)
randomD = random

random64 :: RandomGen g => g -> (Word64, g)
random64 = random

random32to64 :: RandomGen g => g -> (Word64, g)
random32to64 g0 = ((fromIntegral hi `unsafeShiftL` 32) .|. fromIntegral lo, g2)
  where
    (hi, g1) = next g0
    (lo, g2) = next g1

random64to64 :: RandomGen g => g -> (Word64, g)
random64to64 g0 = (fromIntegral i64, g1)
  where
    (i64, g1) = next g0

acRandom64 :: AC.Seed -> (Word64, AC.Seed)
acRandom64 = AC.bounded_random


main :: IO ()
main = do
  let !sz = Sz1 1048576
      -- !stdGen = mkStdGen 2019
      -- !mtGen = pureMT 2019
      -- !xor32Gen = makeXorshift32 (2019 :: Int)
      -- !xor64Gen = makeXorshift64 (2019 :: Int)
      -- !acGen = AC.seed [2019]
      -- !cbrng64Gen = mkCBRNG32 2019
      -- !tfGen = mkTFGen 2019
      -- !pcgGen = PurePCG.initFrozen 2019 0
      -- !pcgFastGen = PureFastPCG.initFrozen 2019
      -- !xor128Gen = Xorshift128Plus.initialize 2019
      -- !pcGen = mkPCGen (2019 :: Int)
      !sm64Gen = SM.mkSMGen 2019
      gens = List.unfoldr (Just . split) sm64Gen
  sm64MutGen <- Random.restore sm64Gen :: IO (MutGen RealWorld SMGen)
  smParWS <- initWorkerStates Par (const (splitMutGen sm64MutGen))
  -- mwcSeqWS <- initWorkerStates Seq (const MWC.createSystemRandom)
  -- mwcParWS <- initWorkerStates Par (const MWC.createSystemRandom)
  -- sfmtSeqWS <- initWorkerStates Seq (const SFMT.createSystemRandom)
  -- sfmtParWS <- initWorkerStates Par (const SFMT.createSystemRandom)
  -- pcgSeqWS <- initWorkerStates Seq (const PCG.createSystemRandom)
  -- pcgFastSeqWS <- initWorkerStates Seq (const FastPCG.createSystemRandom)
  -- pcgParWS <- initWorkerStates Par (const PCG.createSystemRandom)
  -- pcgFastParWS <- initWorkerStates Par (const FastPCG.createSystemRandom)
  -- mtSeqWS <- initWorkerStates Seq (const MT.getStdGen)
  -- mtParWS <- initWorkerStates Par (const MT.getStdGen)
  defaultMain
    [ bgroup
        "Splitmix"
        [ bgroup
            "Word64"
            [ bgroup
                "Seq"
                [ bgroup
                    "Pure"
                    [ bench "System.Random.SplitMix.nextWord64" $
                      nfIO (randomArrayPureSeq sm64Gen SM.nextWord64 sz)
                    , bench "System.Random.random" $
                      nfIO (randomArrayPureSeq sm64Gen random64 sz)
                    , bench "System.Random.genByteArray" $
                      whnf (fst . genByteArray (unSz sz * 8)) sm64Gen
                    ]
                , bgroup
                    "Stateful"
                    [ bench "System.Random.randomM (PureGen)" $
                      nfIO (randomArrayPureGen64 sm64Gen sz)
                    , bench "System.Random.randomM (MutGen)" $
                      nfIO (randomArrayMutGen64 sm64Gen sz)
                    , bench "System.Random.randomM (PrimGen)" $
                      nfIO (randomArrayPrimGen64 sm64Gen Seq sz)
                    ]
                ]
            , bgroup
                "Par"
                [ bgroup
                    "Pure"
                    [ bench "System.Random.SplitMix.nextWord64" $
                      nfIO (randomArrayPurePar sm64Gen SM.nextWord64 sz)
                    , bench "System.Random.random" $
                      nfIO (randomArrayPurePar sm64Gen random64 sz)
                    -- , bench "System.Random.randomM (PureGen)" $
                    --   nfIO (randomArrayPureGen64 sm64Gen sz)
                    ]
                , bgroup
                    "Stateful"
                    [ bench "System.Random.randomM (MutGen)" $
                      nfIO (randomArrayWord64 smParWS sz randomM)
                    --,  bench "System.Random.randomM (PrimGen)" $
                    --   nfIO (randomArrayPrimGen64 sm64Gen Par sz)
                    --
                    ]
                ]
            ]
        ]
    ]
