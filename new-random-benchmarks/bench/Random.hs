{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Proxy
import Data.Typeable
import Criterion.Main
--import Data.Bits
import Data.Int
import Data.PCGen
import Data.Word
--import Lib
import Control.Arrow
import Random.MWC.Primitive as AC
--import Random.MWC.Pure as AC
import Random.Xorshift.Int32
import Random.Xorshift.Int64
import System.Random as Random
import System.Random.Mersenne.Pure64 as PureMT
-- import System.Random.MWC as MWC
-- import System.Random.PCG as PCG
-- import System.Random.PCG.Fast as FastPCG
import System.Random.PCG.Fast.Pure as PureFastPCG
import System.Random.PCG.Pure as PurePCG
import System.Random.SplitMix as SM64
import System.Random.SplitMix32 as SM32
--import System.Random.SFMT as SFMT
import System.Random.TF as TF
--import qualified System.Random.Mersenne as MT
import qualified System.Random.Xorshift128Plus as Xorshift128Plus


instance RandomGen AC.Seed where
#if MIN_VERSION_random(1, 2, 0)
  genWord32 = AC.next_word
#endif
  next !g =
    case next_word g of
      (w32, g') -> (fromIntegral w32, g')
  genRange _ = (0, fromIntegral (maxBound :: Word32))
  split _ = error "AC-Random does not support splitting"

instance RandomGen Xorshift128Plus.Gen where
#if MIN_VERSION_random(1, 2, 0)
  genWord32 = first fromIntegral . Xorshift128Plus.next
  genWord64 = Xorshift128Plus.next
#endif
  next !g =
    case Xorshift128Plus.next g of
      (w64, g') -> (fromIntegral w64, g')
  genRange _ = (fromIntegral (minBound :: Int64), fromIntegral (maxBound :: Int64))
  split _ = error "Xorshift128Plus does not support splitting"

genManyRandom :: forall a g . (Random a, RandomGen g) => g -> Int -> ()
genManyRandom g0 n = go g0 0
  where
    go g i
      | i < n =
        case random @a g of
          (val, g') -> val `seq` go g' (i + 1)
      | otherwise = g `seq` ()

genManyRandomRange :: forall a g . (Bounded a, Enum a, Random a, RandomGen g) => g -> Int -> ()
genManyRandomRange g0 n = go g0 0
  where
    (l, h) = (succ (minBound :: a), pred (maxBound :: a))
    go g i
      | i < n =
        case Random.randomR (l, h) g of
          (val, g') -> val `seq` go g' (i + 1)
      | otherwise = g `seq` ()

-- genManyUniformRange :: forall a g . (Bounded a, Enum a, UniformRange a, RandomGen g) => g -> Int -> ()
-- genManyUniformRange g0 n = go g0 0
--   where
--     (l, h) = (succ (minBound :: a), pred (maxBound :: a))
--     go g i
--       | i < n 
--         case Random.uniformR (l, h) g of
--           (val, g') -> val `seq` go g' (i + 1)
--       | otherwise = g `seq` ()

main :: IO ()
main = do
  let !sz = 1048576
  defaultMain
    [ bgroup "Pure"
      [ bgroup "Uniform"
        [ pureBench @Word8 sz
        , pureBench @Word16 sz
        , pureBench @Word32 sz
        , pureBench @Word64 sz
        , pureBench @Int8 sz
        , pureBench @Int16 sz
        , pureBench @Int32 sz
        , pureBench @Int64 sz
        , pureBench @Char sz
        , pureBench @Bool sz
        , pureBench @Integer sz
        ]
      , bgroup "Range"
        [ pureBenchRange @Word8 sz
        , pureBenchRange @Word16 sz
        , pureBenchRange @Word32 sz
        , pureBenchRange @Word64 sz
        , pureBenchRange @Int8 sz
        , pureBenchRange @Int16 sz
        , pureBenchRange @Int32 sz
        , pureBenchRange @Int64 sz
        , pureBenchRange @Char sz
        ]
      ]
    ]

pureBench ::
     forall a. (Typeable a, Random a)
  => Int
  -> Benchmark
pureBench sz =
  let !stdGen = mkStdGen 2020
      !mtGen = pureMT 2020
      !xor32Gen = makeXorshift32 (2020 :: Int)
      !xor64Gen = makeXorshift64 (2020 :: Int)
      !acGen = AC.seed [2020]
      !tfGen = mkTFGen 2020
      !pcgGen = PurePCG.initFrozen 2020 0
      !pcgFastGen = PureFastPCG.initFrozen 2020
      !xor128Gen = Xorshift128Plus.initialize 2020
      !pcGen = mkPCGen (2020 :: Int)
      !sm32Gen = SM32.mkSMGen 2020
      !sm64Gen = SM64.mkSMGen 2020
   in bgroup
        (showsTypeRep (typeRep (Proxy :: Proxy a)) "")
        [ bench "random" $ nf (genManyRandom @a stdGen) sz
        , bench "mersenne-random-pure64" $ nf (genManyRandom @a mtGen) sz
        , bench "xorshift (32)" $ nf (genManyRandom @a xor32Gen) sz
        , bench "xorshift (64)" $ nf (genManyRandom @a xor64Gen) sz
        , bench "AC-Random" $ nf (genManyRandom @a acGen) sz
        , bench "tf-random" $ nf (genManyRandom @a tfGen) sz
        , bench "pcg-random" $ nf (genManyRandom @a pcgGen) sz
        , bench "pcg-random (fast)" $ nf (genManyRandom @a pcgFastGen) sz
        , bench "Xorshift128Plus" $ nf (genManyRandom @a xor128Gen) sz
        , bench "pcgen" $ nf (genManyRandom @a pcGen) sz
        , bench "splitmix (32)" $ nf (genManyRandom @a sm32Gen) sz
        , bench "splitmix (64)" $ nf (genManyRandom @a sm64Gen) sz
        ]

pureBenchRange ::
     forall a. (Typeable a, Random a, Enum a, Bounded a)
  => Int
  -> Benchmark
pureBenchRange sz =
  let !stdGen = mkStdGen 2020
      !mtGen = pureMT 2020
      !xor32Gen = makeXorshift32 (2020 :: Int)
      !xor64Gen = makeXorshift64 (2020 :: Int)
      !acGen = AC.seed [2020]
      !tfGen = mkTFGen 2020
      !pcgGen = PurePCG.initFrozen 2020 0
      !pcgFastGen = PureFastPCG.initFrozen 2020
      !xor128Gen = Xorshift128Plus.initialize 2020
      !pcGen = mkPCGen (2020 :: Int)
      !sm32Gen = SM32.mkSMGen 2020
      !sm64Gen = SM64.mkSMGen 2020
   in bgroup
        (showsTypeRep (typeRep (Proxy :: Proxy a)) "")
        [ bench "random" $ nf (genManyRandomRange @a stdGen) sz
        , bench "mersenne-random-pure64" $ nf (genManyRandomRange @a mtGen) sz
        , bench "xorshift (32)" $ nf (genManyRandomRange @a xor32Gen) sz
        , bench "xorshift (64)" $ nf (genManyRandomRange @a xor64Gen) sz
        , bench "AC-Random" $ nf (genManyRandomRange @a acGen) sz
        , bench "tf-random" $ nf (genManyRandomRange @a tfGen) sz
        , bench "pcg-random" $ nf (genManyRandomRange @a pcgGen) sz
        , bench "pcg-random (fast)" $ nf (genManyRandomRange @a pcgFastGen) sz
        , bench "Xorshift128Plus" $ nf (genManyRandomRange @a xor128Gen) sz
        , bench "pcgen" $ nf (genManyRandomRange @a pcGen) sz
        , bench "splitmix (32)" $ nf (genManyRandomRange @a sm32Gen) sz
        , bench "splitmix (64)" $ nf (genManyRandomRange @a sm64Gen) sz
        ]

-- mkStatefulBench sz = do
--   mwcGen <- MWC.createSystemRandom
--   sfmtGen <- SFMT.createSystemRandom
--   sfmtParWS <- initWorkerStates Par (const SFMT.createSystemRandom)
--   pcgSeqWS <- initWorkerStates Seq (const PCG.createSystemRandom)
--   pcgFastSeqWS <- initWorkerStates Seq (const FastPCG.createSystemRandom)
--   pcgParWS <- initWorkerStates Par (const PCG.createSystemRandom)
--   pcgFastParWS <- initWorkerStates Par (const FastPCG.createSystemRandom)
--   mtSeqWS <- initWorkerStates Seq (const MT.getStdGen)
--   mtParWS <- initWorkerStates Par (const MT.getStdGen)
--                 , bgroup
--                     "Stateful"
--                     [ bench "sfmt" $
--                       nfIO (randomArrayWord64 sfmtSeqWS sz SFMT.uniform)
--                     , bench "pcg-random (fast)" $
--                       nfIO (randomArrayWord64 pcgFastSeqWS sz FastPCG.uniform)
--                     , bench "pcg-random" $
--                       nfIO (randomArrayWord64 pcgSeqWS sz PCG.uniform)
--                     , bench "mersenne-random" $
--                       nfIO (randomArrayWord64 mtSeqWS sz MT.random)
--                     , bench "mwc-random" $
--                       nfIO (randomArrayWord64 mwcSeqWS sz MWC.uniform)
--                     ]
