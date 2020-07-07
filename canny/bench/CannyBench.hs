{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Codec.Picture as JP
import Control.Concurrent
import Control.DeepSeq
import Criterion.Main
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Examples.Internal as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as A
import Data.Coerce
import Data.Massiv.Array as M
import Data.Massiv.Array.IO as M
import Data.Massiv.Array.Manifest.Vector as M
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Storable as VS
import qualified GHC.Exts as IsList (fromList)
import Prelude as P


import qualified Canny.Accelerate as A
import qualified Canny.Massiv as M
import qualified Canny.Repa as R

fromRepaImageY :: R.Image Word8 -> IO (Image S (Y D65) Word8)
fromRepaImageY imgY = do
  mimg <- fromDynamicImageM $ JP.ImageY8 $ R.toJuicyPixels imgY
  case mimg of
    Just img -> pure img
    Nothing -> error "error converting"

toRepaImage :: Image S (SRGB 'NonLinear) Word8 -> R.Image (Word8, Word8, Word8)
toRepaImage = R.fromJuicyPixels . toJPImageRGB8 . toImageBaseModel

toAccelerateImage :: Image S (SRGB 'NonLinear) Word8 -> A.Image Word32
toAccelerateImage =
  A.fromJuicyPixels .
  toJPImageRGBA8 . toImageBaseModel . M.compute . M.map (liftPixel (`addAlpha` maxValue))

main :: IO ()
main = do
  let !low = 50
      !high = 100
  imgRGB :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/lena.bmp"
  defaultMain
    [ bgroup
        "Canny"
        [ env (pure imgRGB) $ \img ->
            bench "massiv" $ nfIO (M.runCanny low high img)
        , env (pure (toRepaImage imgRGB)) $ \img ->
            bench "repa" $ nfIO (R.runCanny low high img)
        , env (pure (toAccelerateImage imgRGB)) $ \img ->
            bench "accelerate" $ nfIO (A.runCanny A.CPU low high img)
        ]
    ]

