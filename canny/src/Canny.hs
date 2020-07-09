{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Canny where

import qualified Canny.Accelerate as A
import qualified Canny.Friday as F
import qualified Canny.Repa as R
import qualified Canny.Massiv as M
import qualified Canny.Yarr as Y
import qualified Codec.Picture as JP
import Control.Monad
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Examples.Internal as A
import qualified Vision.Image as F
import Data.Massiv.Array as M
import Data.Massiv.Array.IO as M
import qualified Graphics.Pixel as CM
import Prelude as P

applyCanny ::
     (JP.Image JP.PixelRGB8 -> IO a) -- ^ Convert from JuciyPixels RGB image
  -> (b -> IO (JP.Image JP.Pixel8)) -- ^ Convert to JuciyPixels Y image
  -> (a -> IO b) -- ^ Canny implementation
  -> Image S (SRGB 'NonLinear) Word8 -- ^ Source RGB Massiv image
  -> IO (Image S (Y D65) Word8)
applyCanny fromRGB toY runCanny =
  fromImageY <=< toY <=< runCanny <=< fromRGB . toJPImageRGB8 . toImageBaseModel

applyMassivCanny ::
     Float
  -> Float
  -> Image S (SRGB 'NonLinear) Word8
  -> IO (Image S (Y D65) Word8)
applyMassivCanny low high =
  applyCanny
    fromImageRGB
    (pure . toJPImageY8 . toImageBaseModel . toImageY')
    (M.runCanny low high)


applyRepaCanny ::
     Float
  -> Float
  -> Image S (SRGB 'NonLinear) Word8
  -> IO (Image S (Y D65) Word8)
applyRepaCanny low high =
  applyCanny
    (pure . R.fromJuicyPixels)
    (pure . R.toJuicyPixels)
    (R.runCanny low high)

applyYarrCanny ::
     Float
  -> Float
  -> Image S (SRGB 'NonLinear) Word8
  -> IO (Image S (Y D65) Word8)
applyYarrCanny low high =
  applyCanny
    (Y.fromJuicyPixels)
    (pure . Y.toJuicyPixels)
    (Y.runCanny (round low) (round high))

applyFridayCanny ::
     Float
  -> Float
  -> Image S (SRGB 'NonLinear) Word8
  -> IO (Image S (Y D65) Word8)
applyFridayCanny low high =
  applyCanny
    (pure . F.fromJuicyPixels)
    (pure . F.toJuicyPixels)
    (pure . F.runCanny (round low) (round high))



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

fromImageY :: JP.Image JP.Pixel8 -> IO (Image S (Y D65) Word8)
fromImageY imgY = do
  mimg <- fromDynamicImageM $ JP.ImageY8 imgY
  case mimg of
    Just img -> pure img
    Nothing  -> error "error converting Y8"

fromImageRGB :: JP.Image JP.PixelRGB8 -> IO (Image S (SRGB 'NonLinear) Word8)
fromImageRGB imgRGB = do
  mimg <- fromDynamicImageM $ JP.ImageRGB8 imgRGB
  case mimg of
    Just img -> pure img
    Nothing  -> error "error converting RGB8"


fromRepaImageY :: R.Image Word8 -> IO (Image S (Y D65) Word8)
fromRepaImageY imgY = fromImageY $ R.toJuicyPixels imgY

toRepaImageRGB :: Image S (SRGB 'NonLinear) Word8 -> R.Image (Word8, Word8, Word8)
toRepaImageRGB = R.fromJuicyPixels . toJPImageRGB8 . toImageBaseModel

fromYarrImageY :: Y.FImage Word8 -> IO (Image S (Y D65) Word8)
fromYarrImageY imgY = fromImageY $ Y.toJuicyPixels imgY

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


toFridayImageRGB :: Image S (SRGB 'NonLinear) Word8 -> F.RGB
toFridayImageRGB = F.fromJuicyPixels . toJPImageRGB8 . toImageBaseModel
