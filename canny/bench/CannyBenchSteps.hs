{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Criterion.Main
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Examples.Internal as A
import qualified Data.Array.Repa.Repr.Accelerate as A
import Data.Massiv.Array as M
import Data.Massiv.Array.IO as M
import Prelude as P

import Canny
import qualified Canny.Accelerate as A
import qualified Canny.Massiv as M
import qualified Canny.Repa as R
--import qualified Canny.Yarr as Y

main :: IO ()
main = do
  let !low = 50
      !high = 100
  imgRGB :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/frog.jpg"
    --"files/lena.bmp"
  let gradMass = M.toGreyScale imgRGB >>= M.blur >>= M.gradientMagOrient low
      gradRepa = R.toGreyScale (toRepaImageRGB imgRGB) >>= R.blur >>= R.grad low
      gradAcc =
        A.run A.CPU . A.gradientMagDir (A.lift low) . A.use . A.blur A.CPU $
        toAccelerateImageY imgRGB
      supMass = gradMass >>= M.suppress low high
      supRepa = gradRepa >>= R.suppress low high
      supAcc =
        A.run A.CPU . A.nonMaximumSuppression (A.lift low) (A.lift high) . A.use $
        gradAcc
      strongMass = supMass >>= \sup -> (,) sup <$> M.selectStrong sup
      strongRepa = supRepa >>= \sup -> (,) sup <$> R.selectStrong sup
      strongAcc =
        ( A.toRepa supAcc
        , A.toRepa . A.run A.CPU . A.selectStrong . A.use $ supAcc)
  defaultMain
    [ bgroup
        "Grayscale"
        [ env (pure imgRGB) $ \img -> bench "massiv" $ nfIO (M.toGreyScale img)
        , env (pure (toRepaImageRGB imgRGB)) $ \img ->
            bench "repa" $ nfIO (R.toGreyScale img)
        , env (pure (toAccelerateImageRGB imgRGB)) $ \img ->
            bench "accelerate" $ nf (A.run A.CPU . A.toGreyscale . A.use) img
        ]
    , bgroup
        "Blur"
        [ env (M.toGreyScale imgRGB) $ \img ->
            bench "massiv" $ nfIO (M.blur img)
        , env (R.toGreyScale $ toRepaImageRGB imgRGB) $ \img ->
            bench "repa" $ nfIO (R.blur img)
        , env (pure (toAccelerateImageY imgRGB)) $ \img ->
            bench "accelerate" $ nf (A.blur A.CPU) img
        ]
    , bgroup
        "Gradient"
        [ env (M.toGreyScale imgRGB >>= M.blur) $ \img ->
            bench "massiv" $ nfIO (M.gradientMagOrient low img)
        , env (R.toGreyScale (toRepaImageRGB imgRGB) >>= R.blur) $ \img ->
            bench "repa" $ nfIO (R.grad low img)
        , env (pure (A.blur A.CPU $ toAccelerateImageY imgRGB)) $ \img ->
            bench "accelerate" $
            nf (A.run A.CPU . A.gradientMagDir (A.lift low) . A.use) img
        ]
    , bgroup
        "Suppress"
        [ env gradMass $ \img -> bench "massiv" $ nfIO (M.suppress low high img)
        , env gradRepa $ \img -> bench "repa" $ nfIO (R.suppress low high img)
        , env (pure gradAcc) $ \img ->
            bench "accelerate" $
            nf
              (A.run A.CPU .
               A.nonMaximumSuppression (A.lift low) (A.lift high) . A.use)
              img
        ]
    , bgroup
        "Strong"
        [ env supMass $ \img -> bench "massiv" $ nfIO (M.selectStrong img)
        , env supRepa $ \img -> bench "repa" $ nfIO (R.selectStrong img)
        , env (pure supAcc) $ \img ->
            bench "accelerate" $ nf (A.run A.CPU . A.selectStrong . A.use) img
        ]
    , bgroup
        "Wildfire"
        [ env strongMass $ \ ~(sup, strong) ->
            bench "massiv" $ nfIO (M.wildfire sup strong)
        , env strongRepa $ \ ~(sup, strong) ->
            bench "repa" $ nfIO (R.wildfire sup strong)
        , env (pure strongAcc) $ \ ~(sup, strong) ->
            bench "accelerate" $ nfIO (A.wildfire sup strong)
        ]
    ]

