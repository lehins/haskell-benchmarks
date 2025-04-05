{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word
import System.Random.Stateful

data TxId = TxId Word64 !Word64 !Word64 !Word64
  deriving (Eq, Ord, Show)

instance NFData TxId where
  rnf = rwhnf
  {-# INLINE rnf #-}

instance Uniform TxId where
  uniformM gen =
    TxId <$> uniformM gen <*> uniformM gen <*> uniformM gen <*> uniformM gen
  {-# INLINE uniformM #-}

data TxIn = TxIn !TxId !Word16
  deriving (Eq, Ord, Show)

instance NFData TxIn where
  rnf = rwhnf
  {-# INLINE rnf #-}

instance Uniform TxIn where
  uniformM gen = TxIn <$> uniformM gen <*> uniformM gen
  {-# INLINE uniformM #-}

main :: IO ()
main = do
  gen <- newIOGenM $ mkStdGen 2025
  bigList :: [(TxIn, Word)] <- uniformListM 2_000_000 gen
  bigMap <- evaluate $ force $ Map.fromList bigList
  smallList :: [(TxIn, Word)] <- uniformListM 300 gen
  smallMap <- evaluate $ force $ Map.fromList smallList
  let mkSubSet xs n = Set.fromList . map fst . take n <$> uniformShuffleListM xs gen
      isProperSubsetAll, isProperSubsetRestrict :: Map.Map TxIn a -> Set.Set TxIn -> Bool
      isProperSubsetAll m = all (`Map.member` m)
      isProperSubsetRestrict m s = Map.size (Map.restrictKeys m s) == Set.size s
      mkBench n xs m =
        bgroup
          (show n)
          [ env (mkSubSet xs n) $ bench "restrictSet" . nf (isProperSubsetRestrict m)
          , env (mkSubSet xs n) $ bench "all" . nf (isProperSubsetAll m)
          ]
  defaultMain
    [ bgroup
        "Big"
        [ mkBench 1 bigList bigMap
        , mkBench 2 bigList bigMap
        , mkBench 4 bigList bigMap
        , mkBench 16 bigList bigMap
        , mkBench 32 bigList bigMap
        , mkBench 64 bigList bigMap
        , mkBench 128 bigList bigMap
        , mkBench 256 bigList bigMap
        ]
    , bgroup
        "Small"
        [ mkBench 1 smallList smallMap
        , mkBench 2 smallList smallMap
        , mkBench 4 smallList smallMap
        , mkBench 16 smallList smallMap
        , mkBench 32 smallList smallMap
        , mkBench 64 smallList smallMap
        , mkBench 128 smallList smallMap
        , mkBench 256 smallList smallMap
        ]
    ]
