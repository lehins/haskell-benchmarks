{-# LANGUAGE FlexibleContexts #-}

module Canny.Friday where

import qualified Codec.Picture as JP
import Data.Int
import Vision.Detector.Edge (canny)
import Vision.Image as I
import Vision.Primitive.Shape


fromJuicyPixels :: JP.Image JP.PixelRGB8 -> RGB
fromJuicyPixels img@(JP.Image w h _) =
  fromFunction (Z :. h :. w) $ \(Z :. i :. j) ->
    case JP.pixelAt img j i of
      JP.PixelRGB8 r g b -> RGBPixel r g b

toJuicyPixels :: Grey -> JP.Image JP.Pixel8
toJuicyPixels img = JP.generateImage (\x y -> fromGreyPixel $ index img (Z :. y :. x)) n m
  where Z :. m :. n = shape img
        fromGreyPixel (GreyPixel y) = y



runCanny :: Int32 -> Int32 -> RGB -> Grey
runCanny lowThresh highThresh img =
  let blurred, edges, imgGrey :: Grey
      imgGrey =
        flip I.map img $ \(RGBPixel r g b) ->
          fromIntegral
            ((fromIntegral r * 299 + fromIntegral g * 587 + fromIntegral b * 114) `div`
             (1000 :: Word))
      -- Applies a Gaussian filter with a 3x3 Double kernel to remove small noises.
      blurred = gaussianBlur 1 (Nothing :: Maybe Double) imgGrey
      -- Applies the Canny's algorithm with a 3x3 Sobel kernel (radius = 1).
      edges = canny 1 lowThresh highThresh blurred
   in edges
