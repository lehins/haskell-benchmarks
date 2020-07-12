{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as M
import Data.Massiv.Array.IO as M
import Prelude as P

import Canny
import qualified Canny.Accelerate as A
import qualified Canny.Friday as F
import qualified Canny.Massiv as M
import qualified Canny.Repa as R
import qualified Canny.Yarr as Y

main :: IO ()
main = do
  smallImg :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/lena.bmp"
  mediumImg :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/frog.jpg"
  defaultMain
    [bgroup "Small" $ mkGroup smallImg, bgroup "Medium" $ mkGroup mediumImg]

mkGroup :: Image S (SRGB 'NonLinear) Word8 -> [Benchmark]
mkGroup imgRGB =
  [ env (pure imgRGB) $ \img -> bench "massiv" $ nfIO (M.runCanny low high img)
  , env (pure (toRepaImageRGB imgRGB)) $ \img ->
      bench "repa" $ nfIO (R.runCanny low high img)
  , env (pure (toAccelerateImageRGB imgRGB)) $ \img ->
      bench "accelerate" $ nfIO (A.runCanny low high img)
  , env (toYarrImageRGB imgRGB) $ \img ->
      bench "yarr" $ nfIO (Y.runCanny (round low) (round high) img)
  , env (pure $ toFridayImageRGB imgRGB) $ \img ->
      bench "friday" $ nf (F.runCanny (round low) (round high)) img
  ]
  where
    !low = 50
    !high = 100
