{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Canny.Yarr where
-- Copy from https://github.com/leventov/yarr/blob/4167330ec947c84c48ed5d105bc3aac83fa9db8c/tests/canny.hs

import Control.Monad
import Prelude as P

import Data.Int
import Data.Word

import qualified Codec.Picture as JP
import Data.Yarr as Y
import Data.Yarr.Convolution as C
import Data.Yarr.Repr.Foreign
import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Walk

import Foreign.Storable.Tuple ()
import System.IO.Unsafe

deriving instance Storable e => Storable (VecTuple N2 e)

type FImage = UArray F L Dim2
type FImageRGBF = FImage (VecList N3 Float)

fromJuicyPixels :: JP.Image JP.PixelRGB8 -> IO (FImage (VecList N3 Float))
fromJuicyPixels img@(JP.Image w h _) =
  dComputeS $ mapElems fromIntegral $
    fromFunction (h, w) $ \(i, j) ->
      case JP.pixelAt img j i of
        JP.PixelRGB8 r g b -> pure $ vl_3 r g b

toJuicyPixels :: FImage Word8 -> JP.Image JP.Pixel8
toJuicyPixels img = JP.generateImage (\x y -> unsafePerformIO $ Y.index img (y, x)) n m
  where (m, n) = extent img


{-# INLINE floatToWord8 #-}
floatToWord8 :: (RealFrac a, Num b) => a -> b
floatToWord8 f = fromIntegral (truncate f :: Int)


runCanny ::
     Word8
  -> Word8
  -> UArray F L Dim2 (VecList N3 Float)
  -> IO (UArray F L Dim2 Word8)
runCanny threshLow threshHigh image = do
  edges <- newEmpty (extent image)
  process threshLow threshHigh image edges
  return edges

process :: Word8 -> Word8 -> FImage (VecList N3 Float) -> FImage Word8 -> IO ()
process threshLow threshHigh image resultEdges = do

    let luminosity r g b = (0.299 :: Float) * r + 0.587 * g + 0.114 * b
        delayedLum = zipElems luminosity image

    greyImage <-
            compute (loadP (S.unrolledFill n4 noTouch) caps) $
                dmap floatToWord8 delayedLum

    blurred <- compute blur greyImage

    magOrient <- compute (gradientMagOrient threshLow) blurred

    edges <- supress threshHigh magOrient

    wildfire edges resultEdges


blur :: FImage Word8 -> FImage Float -> IO ()
blur image target = do

    let convolvedX :: UArray CV CVL Dim2 Word
        convolvedX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1 4 6 4 1 |]
                (dmap fromIntegral image)

    (cX :: FImage Word16) <- new (extent image)
    loadP (S.unrolledFill n8 touch) caps
              (dmap fromIntegral convolvedX) cX

    let convolvedY :: UArray CV CVL Dim2 Word
        convolvedY =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1
                         4
                         6
                         4
                         1 |]
                (dmap fromIntegral cX)

        norm :: Word -> Float
        {-# INLINE norm #-}
        norm w = (fromIntegral (fromIntegral w :: Int)) / 256

        blurred = dmap norm convolvedY

    loadP (S.unrolledFill n6 touch) caps blurred target

noOrient, posDiag, vert, negDiag, horiz, noEdge, weak, strong :: Word8
noOrient = 0 :: Word8
posDiag  = 64 :: Word8
vert     = 128 :: Word8
negDiag  = 192 :: Word8
horiz    = 255 :: Word8

noEdge = 0 :: Word8
weak   = 128 :: Word8
strong = 255 :: Word8

gradientMagOrient
    :: Word8 -> FImage Float -> FImage (VecTuple N2 Word8) -> IO ()
gradientMagOrient !threshLow image target =
    loadP S.fill caps delayedMagOrient target
  where
    delayedMagOrient = dzip2 magnitudeAndOrient gradientX gradientY

    gradientX =
        dConvolveLinearDim2WithStaticStencil
            [dim2St| -1  0  1
                     -2  0  2
                     -1  0  1 |]
            image

    gradientY =
        dConvolveLinearDim2WithStaticStencil
            [dim2St|  1   2   1
                      0   0   0
                     -1  -2  -1 |]
            image

    magnitudeAndOrient :: Float -> Float -> VecTuple N2 Word8
    magnitudeAndOrient gX gY =
        VT_2 (mag, if mag < threshLow then noOrient else orient gX gY)
      where
        mag = floatToWord8 $ sqrt (gX * gX + gY * gY)

        orient :: Float -> Float -> Word8
        orient x y
            | atan_1q < 0.33 = horiz
            | atan_1q > 0.66 = vert
            | otherwise      = posDiag + diagInv
          where
            rr = y / x
            (r, diagInv) =
                if rr < 0 then (negate rr, negDiag - posDiag) else (rr, 0)
            -- 2nd order Taylor series of atan,
            -- see http://stackoverflow.com/a/14101194/648955
            br = 0.596227 * r
            num = br + (r * r)
            atan_1q = num / (1.0 + br + num)


supress :: Word8 -> FImage (VecTuple N2 Word8) -> IO (FImage Word8)
supress !threshHigh magOrient = do
    let ext = extent magOrient
    supressed <- newEmpty ext

    let mags = V.head (slices magOrient)
        mg = Y.index mags

        {-# INLINE isMax #-}
        isMax sh m m1 m2 = do
            mag1 <- m1
            when (m > mag1) $ do
                mag2 <- m2
                when (m > mag2) $ do
                     let e = if m < threshHigh then weak else strong
                     write supressed sh e

        {-# INLINE comparePts #-}
        comparePts sh@(y, x) (VT_2 (m, o))
            | o == noOrient= return ()
            | o == horiz   = isMax sh m (mg (y,     x - 1)) (mg (y,     x + 1))
            | o == vert    = isMax sh m (mg (y - 1, x    )) (mg (y + 1, x    ))
            | o == negDiag = isMax sh m (mg (y - 1, x - 1)) (mg (y + 1, x + 1))
            | o == posDiag = isMax sh m (mg (y - 1, x + 1)) (mg (y + 1, x - 1))
            | otherwise    = return ()

        shapedSupressed =
            ShapeDelayedTarget
                ext (touchArray supressed) (force supressed) comparePts

        {-# INLINE supressLoad #-}
        supressLoad arr tarr =
            rangeLoadP (S.dim2BlockFill n3 n3 touch) caps arr tarr
                       (1, 1) (ext `minus` (1, 1))

    supressLoad magOrient shapedSupressed
    return supressed


wildfire :: FImage Word8 -> FImage Word8 -> IO ()
wildfire edges target = do
    let ext@(h, w) = extent edges
        newStack :: IO (UArray F L Dim1 (VecTuple N2 Int16))
        newStack = new (h * w)

        {-# INLINE stackIndex #-}
        stackIndex stack i = do
            ix16 <- Y.index stack i
            let (VT_2 ix) = V.map fromIntegral ix16
            return ix

        {-# INLINE stackWrite #-}
        stackWrite stack i ix =
            linearWrite stack i (V.map fromIntegral (VT_2 ix))

        {-# INLINE pushWeak #-}
        pushWeak stack ix top = do
            edge <- Y.index edges ix
            if edge == noEdge
                then return top
                else do
                    stackWrite stack top ix
                    write edges ix noEdge
                    return (top + 1)

        {-# INLINE fire #-}
        fire stack top
            | top == 0  = return ()
            | otherwise = do
                let top' = top - 1
                ix@(y, x) <- stackIndex stack top'
                write target ix strong
                    >>  pushWeak stack (y - 1, x - 1) top'
                    >>= pushWeak stack (y - 1, x)
                    >>= pushWeak stack (y - 1, x + 1)

                    >>= pushWeak stack (y,     x - 1)
                    >>= pushWeak stack (y,     x + 1)

                    >>= pushWeak stack (y + 1, x - 1)
                    >>= pushWeak stack (y + 1, x)
                    >>= pushWeak stack (y + 1, x + 1)

                    >>= fire stack

        {-# INLINE tryFire #-}
        tryFire stack ix edge
            | edge /= strong = return ()
            | otherwise      = do
                stackWrite stack 0 ix
                write edges ix noEdge
                fire stack 1

    lastStack <-
        rangeWalkP caps
                (imutate (S.unrolledFill n4 noTouch) tryFire)
                newStack (\s1 s2 -> touchArray s1 >> return s2)
                edges (1, 1) (ext `minus` (1, 1))
    touchArray lastStack
    touchArray target
