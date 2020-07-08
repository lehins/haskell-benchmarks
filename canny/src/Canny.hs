{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Canny where

import qualified Codec.Picture as JP
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Examples.Internal as A
import Data.Massiv.Array as M
import Data.Massiv.Array.IO as M
import qualified Graphics.Pixel as CM
import Prelude as P

import qualified Canny.Accelerate as A
import qualified Canny.Repa as R
import qualified Canny.Yarr as Y

toImageY ::
     Array S Ix2 (Pixel (SRGB 'NonLinear) Word8)
  -> Array S Ix2 (Pixel CM.Y Float)
toImageY =
  toImageBaseModel .
  M.compute .
  M.map ((\(PixelY' x) -> PixelY x :: Pixel (Y D65) Float) . rgbPixelLuma)

toImageY' ::
     Array S Ix2 (Pixel Y' Word8)
  -> Array S Ix2 (Pixel (Y D65) Word8)
toImageY' = M.compute . M.map ((\(PixelY' x) -> PixelY x :: Pixel (Y D65) Word8))

fromRepaImageY :: R.Image Word8 -> IO (Image S (Y D65) Word8)
fromRepaImageY imgY = do
  mimg <- fromDynamicImageM $ JP.ImageY8 $ R.toJuicyPixels imgY
  case mimg of
    Just img -> pure img
    Nothing  -> error "error converting"

toRepaImageRGB :: Image S (SRGB 'NonLinear) Word8 -> R.Image (Word8, Word8, Word8)
toRepaImageRGB = R.fromJuicyPixels . toJPImageRGB8 . toImageBaseModel

fromYarrImageY :: Y.FImage Word8 -> IO (Image S (Y D65) Word8)
fromYarrImageY imgY = do
  mimg <- fromDynamicImageM $ JP.ImageY8 $ Y.toJuicyPixels imgY
  case mimg of
    Just img -> pure img
    Nothing  -> error "error converting"

toYarrImageRGB :: Image S (SRGB 'NonLinear) Word8 -> IO Y.FImageRGBF
toYarrImageRGB = Y.fromJuicyPixels . toJPImageRGB8 . toImageBaseModel

toAccelerateImageRGB :: Image S (SRGB 'NonLinear) Word8 -> A.Image Word32
toAccelerateImageRGB =
  A.fromJuicyPixels .
  toJPImageRGBA8 . toImageBaseModel . M.compute . M.map (liftPixel (`addAlpha` maxValue))

toRepaImageY :: Image S (SRGB 'NonLinear) Word8 -> IO (R.Image Float)
toRepaImageY = R.toGreyScale . toRepaImageRGB

toAccelerateImageY :: Image S (SRGB 'NonLinear) Word8 -> A.Image Float
toAccelerateImageY = A.run A.CPU . A.toGreyscale . A.use . toAccelerateImageRGB
