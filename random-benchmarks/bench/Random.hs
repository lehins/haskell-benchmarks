{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Scheduler
import Criterion.Main
import Crypto.Cipher.ChaCha as ChaCha
import Data.Bits
import Data.ByteArray as BA
import Data.ByteString as BS
import Data.ByteString.Builder as BS
import Data.ByteString.Lazy as BL
import Data.Int
import Data.IORef
import Data.Massiv.Array
import Data.PCGen
import Data.Word
import Foreign.Storable (peek)
import Lib
import Random.MWC.Primitive as AC
import Random.MWC.Pure as AC
import Random.Xorshift.Int32
import Random.Xorshift.Int64
import System.Random
import System.Random.Mersenne.Pure64 as PureMT
import System.Random.MWC as MWC
import System.Random.PCG as PCG
import System.Random.PCG.Fast as FastPCG
import System.Random.PCG.Fast.Pure as PureFastPCG
import System.Random.PCG.Pure as PurePCG
import System.Random.Random123
import System.Random.SplitMix as SM64
import System.Random.SplitMix32 as SM32
import System.Random.SFMT as SFMT
import System.Random.TF as TF
import qualified System.Random.Mersenne as MT
import qualified System.Random.Xorshift128Plus as Xorshift128Plus
import Text.Printf

chaChaNext :: Integral i => IORef ChaCha.StateSimple -> IO i
chaChaNext ref = do
    !st <- readIORef ref
    let (ba, st') = ChaCha.generateSimple st 8 :: (BA.Bytes, ChaCha.StateSimple)
    !w64 <- BA.withByteArray ba (\ptr -> do peek ptr :: IO Word64)
    writeIORef ref st'
    return $ fromIntegral w64

mkChaCha :: ChaCha.StateSimple
mkChaCha = ChaCha.initializeSimple $ BS.concat . BL.toChunks $ BS.toLazyByteString $ BS.string7 "please insert 40 bytes of randomness here"

theChaChaState :: IO (IORef ChaCha.StateSimple)
theChaChaState = newIORef mkChaCha
{-# NOINLINE theChaChaState #-}

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


printRanges :: IO ()
printRanges =
  traverse_
    (\(pkg, ty, (from, to)) -> printf "[%20d, %20d] - %s:%s\n" from to pkg ty)
    [ ("random", "System.Random.StdGen", genRange (mkStdGen 2019))
    , ( "mersenne-random-pure64"
      , "System.Random.Mersenne.Pure64.MTGen"
      , genRange (pureMT 2019))
    , ( "xorshift"
      , "Random.Xorshift.Int32.Xorshift32"
      , genRange (makeXorshift32 (2019 :: Int)))
    , ( "xorshift"
      , "Random.Xorshift.Int64.Xorshift64"
      , genRange (makeXorshift64 (2019 :: Int)))
    , ("AC-Random", "Random.MWC.Primitive.Seed", genRange (AC.seed [2019]))
    , ("tf-random", "System.Random.TF.TFGen", genRange (mkTFGen 2019))
    , ( "pcg-random"
      , "System.Random.PCG.Pure.SetSeq"
      , genRange (PurePCG.initFrozen 2019 0))
    , ( "pcg-random"
      , "System.Random.PCG.Fast.Pure.FrozenGen"
      , genRange (PureFastPCG.initFrozen 2019))
    , ( "Xorshift128Plus"
      , "System.Random.Xorshift128Plus.Gen"
      , genRange (Xorshift128Plus.initialize 2019))
    , ("pcgen", "Data.PCGen.PCGen", genRange (mkPCGen (2019 :: Int)))
    , ("splitmix", "Data.SplitMix32.SMGen", genRange (SM32.mkSMGen 2019))
    , ("splitmix", "Data.SplitMix.SMGen", genRange (SM64.mkSMGen 2019))
    ]

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
  sm64Gen <- SM64.newSMGen
  let !sz = Sz1 1048576
      !stdGen = mkStdGen 2019
      !mtGen = pureMT 2019
      !xor32Gen = makeXorshift32 (2019 :: Int)
      !xor64Gen = makeXorshift64 (2019 :: Int)
      !acGen = AC.seed [2019]
      !cbrng64Gen = mkCBRNG32 2019
      !tfGen = mkTFGen 2019
      !pcgGen = PurePCG.initFrozen 2019 0
      !pcgFastGen = PureFastPCG.initFrozen 2019
      !xor128Gen = Xorshift128Plus.initialize 2019
      !pcGen = mkPCGen (2019 :: Int)
      !sm32Gen = SM32.mkSMGen 2019
  mwcSeqWS <- initWorkerStates Seq (const MWC.createSystemRandom)
  mwcParWS <- initWorkerStates Par (const MWC.createSystemRandom)
  sfmtSeqWS <- initWorkerStates Seq (const SFMT.createSystemRandom)
  sfmtParWS <- initWorkerStates Par (const SFMT.createSystemRandom)
  pcgSeqWS <- initWorkerStates Seq (const PCG.createSystemRandom)
  pcgFastSeqWS <- initWorkerStates Seq (const FastPCG.createSystemRandom)
  pcgParWS <- initWorkerStates Par (const PCG.createSystemRandom)
  pcgFastParWS <- initWorkerStates Par (const FastPCG.createSystemRandom)
  mtSeqWS <- initWorkerStates Seq (const MT.getStdGen)
  mtParWS <- initWorkerStates Par (const MT.getStdGen)
  ccSeqWS <- initWorkerStates Seq (const theChaChaState)
  defaultMain
    [ bgroup
        "Disqualified/next/Seq"
        [ bench "random" $ nfIO (randomArrayPureSeq stdGen next sz)
        , bench "Random123" $ nfIO (randomArrayPureSeq cbrng64Gen next sz)
        ]
    , bgroup
        "Pure"
        [ bgroup
            "next/Seq"
            [ bench "random" $ nfIO (randomArrayPureSeq stdGen next sz)
            , bench "mersenne-random-pure64" $
              nfIO (randomArrayPureSeq mtGen next sz)
            , bench "xorshift (32)" $ nfIO (randomArrayPureSeq xor32Gen next sz)
            , bench "xorshift (64)" $ nfIO (randomArrayPureSeq xor64Gen next sz)
            , bench "AC-Random" $ nfIO (randomArrayPureSeq acGen next sz)
            , bench "tf-random" $ nfIO (randomArrayPureSeq tfGen next sz)
            , bench "pcg-random" $ nfIO (randomArrayPureSeq pcgGen next sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPureSeq pcgFastGen next sz)
            , bench "Xorshift128Plus" $
              nfIO (randomArrayPureSeq xor128Gen next sz)
            , bench "pcgen" $ nfIO (randomArrayPureSeq pcGen next sz)
            , bench "splitmix (32)" $ nfIO (randomArrayPureSeq sm32Gen next sz)
            , bench "splitmix (64)" $ nfIO (randomArrayPureSeq sm64Gen next sz)
            ]
        , bgroup
            "next/Par"
            [ bench "random" $ nfIO (randomArrayPurePar stdGen next sz)
            , bench "tf-random" $ nfIO (randomArrayPurePar tfGen next sz)
            , bench "pcg-random" $ nfIO (randomArrayPurePar pcgGen next sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPurePar pcgFastGen next sz)
            , bench "pcgen" $ nfIO (randomArrayPurePar pcGen next sz)
            , bench "splitmix (32)" $ nfIO (randomArrayPurePar sm32Gen next sz)
            , bench "splitmix (64)" $ nfIO (randomArrayPurePar sm64Gen next sz)
            ]
        , bgroup
            "random/Word64/Seq"
            [ bench "random" $ nfIO (randomArrayPureSeq stdGen random64 sz)
            , bench "mersenne-random-pure64" $
              nfIO (randomArrayPureSeq mtGen random64 sz)
            , bench "xorshift (32)" $
              nfIO (randomArrayPureSeq xor32Gen random64 sz)
            , bench "xorshift (64)" $
              nfIO (randomArrayPureSeq xor64Gen random64 sz)
            , bench "AC-Random" $ nfIO (randomArrayPureSeq acGen random64 sz)
            , bench "tf-random" $ nfIO (randomArrayPureSeq tfGen random64 sz)
            , bench "pcg-random" $ nfIO (randomArrayPureSeq pcgGen random64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPureSeq pcgFastGen random64 sz)
            , bench "Xorshift128Plus" $
              nfIO (randomArrayPureSeq xor128Gen random64 sz)
            , bench "pcgen" $ nfIO (randomArrayPureSeq pcGen random64 sz)
            , bench "splitmix (32)" $
              nfIO (randomArrayPureSeq sm32Gen random64 sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPureSeq sm64Gen random64 sz)
            ]
        , bgroup
            "random/Word64/Par"
            [ bench "random" $ nfIO (randomArrayPurePar stdGen random64 sz)
            , bench "tf-random" $ nfIO (randomArrayPurePar tfGen random64 sz)
            , bench "pcg-random" $ nfIO (randomArrayPurePar pcgGen random64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPurePar pcgFastGen random64 sz)
            , bench "pcgen" $ nfIO (randomArrayPurePar pcGen random64 sz)
            , bench "splitmix (32)" $
              nfIO (randomArrayPurePar sm32Gen random64 sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPurePar sm64Gen random64 sz)
            ]
        , bgroup
            "custom/Word64/Seq"
            [ bench "random" $ nfIO (randomArrayPureSeq stdGen random64 sz)
            , bench "mersenne-random-pure64" $
              nfIO (randomArrayPureSeq mtGen PureMT.randomWord64 sz)
            , bench "xorshift (32)" $
              nfIO (randomArrayPureSeq xor32Gen random32to64 sz)
            , bench "xorshift (64)" $
              nfIO (randomArrayPureSeq xor64Gen random64to64 sz)
            , bench "AC-Random" $ nfIO (randomArrayPureSeq acGen acRandom64 sz)
            , bench "pcg-random" $
              nfIO (randomArrayPureSeq pcgGen random64to64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPureSeq pcgFastGen random64to64 sz)
            , bench "Xorshift128Plus" $
              nfIO (randomArrayPureSeq xor128Gen Xorshift128Plus.next sz)
            , bench "pcgen" $ nfIO (randomArrayPureSeq pcGen random32to64 sz)
            , bench "splitmix (32)" $
              nfIO (randomArrayPureSeq sm32Gen SM32.nextWord64 sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPureSeq sm64Gen SM64.nextWord64 sz)
            ]
        , bgroup
            "custom/Word64/Par"
            [ bench "random" $ nfIO (randomArrayPurePar stdGen random64 sz)
            , bench "pcg-random" $
              nfIO (randomArrayPurePar pcgGen random64to64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPurePar pcgFastGen random64to64 sz)
            , bench "pcgen" $ nfIO (randomArrayPurePar pcGen random32to64 sz)
            , bench "splitmix (32)" $
              nfIO (randomArrayPurePar sm32Gen SM32.nextWord64 sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPurePar sm64Gen SM64.nextWord64 sz)
            ]
        , bgroup
            "fast/Word64/Seq"
            [ bench "mersenne-random-pure64" $
              nfIO (randomArrayPureSeq mtGen PureMT.randomWord64 sz)
            , bench "xorshift (32)" $
              nfIO (randomArrayPureSeq xor32Gen random32to64 sz)
            , bench "xorshift (64)" $
              nfIO (randomArrayPureSeq xor64Gen random64to64 sz)
            , bench "pcg-random" $
              nfIO (randomArrayPureSeq pcgGen random64to64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPureSeq pcgFastGen random64to64 sz)
            , bench "Xorshift128Plus" $
              nfIO (randomArrayPureSeq xor128Gen Xorshift128Plus.next sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPureSeq sm64Gen SM64.nextWord64 sz)
            ]
        , bgroup
            "fast/Word64/Par"
            [ bench "pcg-random" $
              nfIO (randomArrayPurePar pcgGen random64to64 sz)
            , bench "pcg-random (fast)" $
              nfIO (randomArrayPurePar pcgFastGen random64to64 sz)
            , bench "splitmix (64)" $
              nfIO (randomArrayPurePar sm64Gen SM64.nextWord64 sz)
            ]
        ]
    , bgroup
        "IO"
        [ bgroup
            "Word64"
            [ bgroup
                "Seq"
                [ bench "mwc-random" $
                  nfIO (randomArrayWord64 mwcSeqWS sz MWC.uniform)
                , bench "sfmt" $
                  nfIO (randomArrayWord64 sfmtSeqWS sz SFMT.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayWord64 pcgSeqWS sz PCG.uniform)
                , bench "pcg-random (fast)" $
                  nfIO (randomArrayWord64 pcgFastSeqWS sz FastPCG.uniform)
                , bench "mersenne-random" $
                  nfIO (randomArrayWord64 mtSeqWS sz MT.random)
                , bench "chacha" $
                  nfIO (randomArrayWord64 ccSeqWS sz chaChaNext)
                ]
            , bgroup
                "Par"
                [ bench "mwc-random" $
                  nfIO (randomArrayWord64 mwcParWS sz MWC.uniform)
                , bench "sfmt" $
                  nfIO (randomArrayWord64 sfmtParWS sz SFMT.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayWord64 pcgParWS sz PCG.uniform)
                , bench "pcg-random (fast)" $
                  nfIO (randomArrayWord64 pcgFastParWS sz FastPCG.uniform)
                , bench "mersenne-random" $
                  nfIO (randomArrayWord64 mtParWS sz MT.random)
                ]
            ]
        ]
    , bgroup
        "Finalists"
        [ bgroup
            "Word64"
            [ bgroup
                "Seq"
                [ bgroup
                    "Pure"
                    [ bench "splitmix (64)" $
                      nfIO (randomArrayPureSeq sm64Gen SM64.nextWord64 sz)
                    , bench "xorshift (64)" $
                      nfIO (randomArrayPureSeq xor64Gen random64to64 sz)
                    , bench "Xorshift128Plus" $
                      nfIO
                        (randomArrayPureSeq xor128Gen Xorshift128Plus.next sz)
                    , bench "pcg-random (fast)" $
                      nfIO (randomArrayPureSeq pcgFastGen random64to64 sz)
                    , bench "pcg-random" $
                      nfIO (randomArrayPureSeq pcgGen random64to64 sz)
                    ]
                , bgroup
                    "Stateful"
                    [ bench "sfmt" $
                      nfIO (randomArrayWord64 sfmtSeqWS sz SFMT.uniform)
                    , bench "pcg-random (fast)" $
                      nfIO (randomArrayWord64 pcgFastSeqWS sz FastPCG.uniform)
                    , bench "pcg-random" $
                      nfIO (randomArrayWord64 pcgSeqWS sz PCG.uniform)
                    , bench "mersenne-random" $
                      nfIO (randomArrayWord64 mtSeqWS sz MT.random)
                    , bench "mwc-random" $
                      nfIO (randomArrayWord64 mwcSeqWS sz MWC.uniform)
                    ]
                ]
            , bgroup
                "Par"
                [ bgroup
                    "Pure"
                    [ bench "splitmix (64)" $
                      nfIO (randomArrayPurePar sm64Gen SM64.nextWord64 sz)
                    , bench "pcg-random (fast)" $
                      nfIO (randomArrayPurePar pcgFastGen random64to64 sz)
                    , bench "pcg-random" $
                      nfIO (randomArrayPurePar pcgGen random64to64 sz)
                    ]
                , bgroup
                    "Stateful"
                    [ bench "sfmt" $
                      nfIO (randomArrayWord64 sfmtParWS sz SFMT.uniform)
                    , bench "mwc-random" $
                      nfIO (randomArrayWord64 mwcParWS sz MWC.uniform)
                    ]
                ]
            ]
        , bgroup
            "Double"
            [ bgroup
                "Seq"
                [ bgroup
                    "Pure"
                    [ bench "splitmix (64)" $
                      nfIO (randomArrayPureSeq sm64Gen SM64.nextDouble sz)
                    , bench "Xorshift128Plus" $
                      nfIO
                        (randomArrayPureSeq xor128Gen Xorshift128Plus.next01 sz)
                    ]
                , bgroup
                    "Stateful"
                    [ bench "pcg-random (fast)" $
                      nfIO (randomArrayDouble pcgFastSeqWS sz FastPCG.uniform)
                    , bench "pcg-random" $
                      nfIO (randomArrayDouble pcgSeqWS sz PCG.uniform)
                    , bench "sfmt" $
                      nfIO (randomArrayDouble sfmtSeqWS sz SFMT.uniform)
                    , bench "mwc-random" $
                      nfIO (randomArrayDouble mwcSeqWS sz MWC.uniform)
                    , bench "mersenne-random" $
                      nfIO (randomArrayDouble mtSeqWS sz MT.random)
                    ]
                ]
            , bgroup
                "Par"
                [ bgroup
                    "Pure"
                    [ bench "splitmix (64)" $
                      nfIO (randomArrayPurePar sm64Gen SM64.nextDouble sz)
                    ]
                , bgroup
                    "Stateful"
                    [ bench "sfmt" $
                      nfIO (randomArrayDouble sfmtParWS sz SFMT.uniform)
                    , bench "mwc-random" $
                      nfIO (randomArrayDouble mwcParWS sz MWC.uniform)
                    ]
                ]
            ]
        ]
    ]
