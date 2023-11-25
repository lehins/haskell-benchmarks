{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main

import Data.List (nub, sort)
import qualified Data.List.Extra as Extra
import qualified Data.Set as Set
import qualified RIO.Prelude as RIO (nubOrd)
import System.Random

main :: IO ()
main = do
  let xs :: [Word]
      xs = take 10000 $ randoms (mkStdGen 2022)
      xsWithDups = take 10000 $ cycle (take 100 xs)
  defaultMain
    [ bgroup
        "nub"
        [ env (pure xs) $ bgroup "Uniform" . benchNub
        , env (pure (sort xs)) $ bgroup "Uniform Sorted" . benchNub
        , env (pure xsWithDups) $ bgroup "Duplicates" . benchNub
        ]
    , bgroup
        "nubSort"
        [ env (pure xs) $ bgroup "Uniform" . benchNubSort
        , env (pure (sort xs)) $ bgroup "Uniform Sorted" . benchNubSort
        , env (pure xsWithDups) $ bgroup "Duplicates" . benchNubSort
        ]
    , bgroup
        "nubSorted"
        [ env (pure (sort xs)) $ bgroup "Uniform Sorted" . benchNubSorted
        , env (pure xsWithDups) $ bgroup "Duplicates" . benchNubSorted
        ]
    ]

-- Benchmark, when we just want to remove duplicates, without loosing the relative order
benchNub :: [Word] -> [Benchmark]
benchNub xs =
  [ bench "nub" $ nf nub xs
  , bench "RIO.nubOrd" $ nf RIO.nubOrd xs
  , bench "Extra.nubOrd" $ nf Extra.nubOrd xs
  , bench "Set nubOrd" $ nf (fst . foldr setNubOrd ([], Set.empty)) xs
  ]
  where
    setNubOrd x acc@(!res, !seen)
      | x `Set.member` seen = acc
      | otherwise = (x : res, Set.insert x seen)

-- Benchmark, when we want to sort and remove duplicates
benchNubSort :: [Word] -> [Benchmark]
benchNubSort xs =
  [ bench "nub . sort" $ nf (nub . sort) xs
  , bench "Extra.nubSort" $ nf Extra.nubSort xs
  , bench "Set.toAscList . Set.fromList" $ nf (Set.toAscList . Set.fromList) xs
  ]

-- Benchmark, when it is known that input is sorted and we just need to remove duplicates
benchNubSorted :: [Word] -> [Benchmark]
benchNubSorted xs =
  [ bench "nub" $ nf nub xs
  , bench "Extra.nubOrd" $ nf Extra.nubOrd xs
  , bench "RIO.nubOrd" $ nf RIO.nubOrd xs
  , bench "Set.toAscList . Set.fromAscList" $ nf (Set.toAscList . Set.fromAscList) xs
  ]
