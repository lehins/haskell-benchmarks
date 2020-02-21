{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Criterion.Main
import Control.Scheduler
import Control.Monad
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import qualified Data.Vector.Primitive as VP
import Prelude as P

loopShortM :: Monad m => Int -> (Int -> a -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc
      | condition step acc = f step acc >>= go (increment step)
      | otherwise = return acc
{-# INLINE loopShortM #-}


anyP :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyP f arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  results <-
    withScheduler (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            res <-
              loopShortM start (\i a -> not a && i < start + chunkLength) (+ 1) False $ \ !i acc ->
                pure (acc || f (unsafeLinearIndex arr i))
            if res
              then void (terminateWith scheduler True) >> yield >> pure True
              else pure res
            -- loopM_ start (< start + chunkLength) (+ 1) $ \ !i ->
            --   when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
            -- pure False
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
              when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
            pure False
  pure $ F.foldl' (||) False results
{-# INLINE anyP #-}


arrayLate :: Array P Ix1 Int
arrayLate =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 7000000, A.singleton 2, A.enumFromStepN Seq 1 2 999999]


arrayLateShort :: Array P Ix1 Int
arrayLateShort =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 7000, A.singleton 2, A.enumFromStepN Seq 1 2 999]

arrayEarly :: Array P Ix1 Int
arrayEarly =
  computeAs P $
  concat' 1 [A.singleton 2, A.enumFromStepN Seq 1 2 7000000, A.enumFromStepN Seq 1 2 999999]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "any"
        [ env (pure (arrayLate, A.toVector arrayLate, A.toList arrayLate)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "Late"
              [ bench "list" $ whnf (P.any even) ls
              , bench "vector" $ whnf (VP.any even) vecVP
              , bench "massiv (Seq)" $ whnf (A.any even) (setComp Seq vec)
              , bench "massiv (Par)" $ whnf (A.any even) (setComp Par vec)
              ]
        , env
            (pure
               ( arrayLateShort
               , A.toVector arrayLateShort
               , A.toList arrayLateShort)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "LateShort"
              [ bench "list" $ whnf (P.any even) ls
              , bench "vector" $ whnf (VP.any even) vecVP
              , bench "massiv (Seq)" $ whnf (A.any even) (setComp Seq vec)
              , bench "massiv (Par)" $ whnf (A.any even) (setComp Par vec)
              ]
        , env (pure (arrayEarly, A.toVector arrayEarly, A.toList arrayEarly)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "Early"
              [ bench "list" $ whnf (P.any even) ls
              , bench "vector" $ whnf (VP.any even) vecVP
              , bench "massiv (Seq)" $ whnf (A.any even) (setComp Seq vec)
              , bench "massiv (Par)" $ whnf (A.any even) (setComp Par vec)
              ]
        ]
    ]






-- anyP :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP f arr = do
--     let sz = size arr
--         totalLength = totalElem sz
--     results <-
--       withScheduler (getComp arr) $ \scheduler ->
--         splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--           loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--             scheduleWork scheduler $ do
--               loopM_ start (< start + chunkLength) (+ 1) $ \ !i ->
--                 when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
--               pure False
--           when (slackStart < totalLength) $
--             scheduleWork scheduler $ do
--               loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
--                 when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
--               pure False
--     pure $ F.foldl' (||) False results
-- {-# INLINE anyP #-}
