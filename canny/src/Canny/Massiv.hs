{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Canny.Massiv where
-- | Canny edge detector.
--
--   NOTE: for best performance this needs to be compiled with the following GHC options:
--         -fllvm -optlo-O3 -Odph -fno-liberate-case
--         -funfolding-use-threshold100 -funfolding-keeness-factor100
--
import Control.Monad
import Data.Int
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Array.IO as A
import GHC.Exts
import Prelude hiding (compare)
import Control.Applicative


-- Constants ------------------------------------------------------------------
orientUndef, orientPosDiag, orientVert, orientNegDiag, orientHoriz :: Word8
orientUndef     = 0     :: Word8
orientPosDiag   = 64    :: Word8
orientVert      = 128   :: Word8
orientNegDiag   = 192   :: Word8
orientHoriz     = 255   :: Word8

edgeNone, edgeWeak, edgeStrong :: Pixel Y' Word8
edgeNone   = 0     :: Pixel Y' Word8
edgeWeak   = 128   :: Pixel Y' Word8
edgeStrong = 255   :: Pixel Y' Word8


runCanny :: Float -> Float -> Image S (SRGB 'NonLinear) Word8 -> IO (Image S Y' Word8)
runCanny threshLow threshHigh arrInput = do
  arrGrey <- toGreyScale arrInput
  arrBluredX <- blurSepX arrGrey
  arrBlured <- blurSepY arrBluredX
  arrDX <- gradientX arrBlured
  arrDY <- gradientY arrBlured
  arrMagOrient <- gradientMagOrient threshLow arrDX arrDY
  arrSuppress <- suppress threshLow threshHigh arrMagOrient
  arrStrong <- selectStrong arrSuppress
  wildfire arrSuppress arrStrong

runCanny' :: Float -> Float -> Image S (SRGB 'NonLinear) Word8 -> IO (Image S Y' Word8)
runCanny' threshLow threshHigh arrInput = do
  arrGrey <- toGreyScale arrInput
  arrBluredX <- blurSepX arrGrey
  arrBlured <- blurSepY arrBluredX
  arrMagOrient <- gradientMagOrient' threshLow arrBlured
  arrSuppress <- suppress' threshLow threshHigh arrMagOrient
  arrStrong <- selectStrong arrSuppress
  wildfire arrSuppress arrStrong


blur :: Image S Y' Float -> IO (Image S Y' Float)
blur = blurSepX >=> blurSepY
{-# INLINE blur #-}


grad :: Float -> Image S Y' Float -> IO (Array S Ix2 Float, Array S Ix2 Word8)
grad thresh img = do
  x <- gradientX img
  y <- gradientY img
  gradientMagOrient thresh x y
{-# INLINE grad #-}

-------------------------------------------------------------------------------
-- | SRGB to greyscale conversion.
toGreyScale :: Image S (SRGB 'NonLinear) Word8 -> IO (Image S Y' Float)
toGreyScale = computeIO . A.map rgbPixelLuma
{-# INLINE toGreyScale #-}

-- | Separable Gaussian blur in the X direction.
blurSepX :: Image S Y' Float -> IO (Image S Y' Float)
blurSepX = computeIO . mapStencil Edge
  (makeStencil (Sz2 1 5) (0 :. 2) $ \get ->
       get (0 :. -2)     +
       get (0 :. -1) * 4 +
       get (0 :.  0) * 6 +
       get (0 :.  1) * 4 +
       get (0 :.  2)
  )
{-# INLINE blurSepX #-}


-- | Separable Gaussian blur in the Y direction.
blurSepY :: Image S Y' Float -> IO (Image S Y' Float)
blurSepY = computeIO . mapStencil Edge
  (makeStencil (Sz2 5 1) (2 :. 0) $ \get ->
       get (-2 :. 0)     +
       get (-1 :. 0) * 4 +
       get ( 0 :. 0) * 6 +
       get ( 1 :. 0) * 4 +
       get ( 2 :. 0)
  )
{-# INLINE blurSepY #-}

-- | Compute gradient in the x direction.
gradientX :: Image S Y' Float -> IO (Image S Y' Float)
gradientX = computeIO . mapStencil Edge sobelX
{-# INLINE gradientX #-}


-- | Compute gradient in the y direction.
gradientY :: Image S Y' Float -> IO (Image S Y' Float)
gradientY = computeIO . mapStencil Edge sobelY
{-# INLINE gradientY #-}

sobelX :: Stencil Ix2 (Pixel Y' Float) (Pixel Y' Float)
sobelX = A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
                f (-1 :. -1) -     f (-1 :.  1) +
            2 * f ( 0 :. -1) - 2 * f ( 0 :.  1) +
                f ( 1 :. -1) -     f ( 1 :.  1)
{-# INLINE sobelX #-}


sobelY :: Stencil Ix2 (Pixel Y' Float) (Pixel Y' Float)
sobelY =  A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
           f (-1 :. -1) + 2 * f (-1 :. 0) + f (-1 :. 1)
         - f ( 1 :. -1) - 2 * f ( 1 :. 0) - f ( 1 :. 1)
{-# INLINE sobelY #-}


-- | Classify the magnitude and orientation of the vector gradient.
gradientMagOrient ::
     Float
  -> Image S Y' Float
  -> Image S Y' Float
  -> IO (Array S Ix2 Float, Array S Ix2 Word8)
gradientMagOrient !threshLow !dX !dY = pure (mag, orient)
  where
    !mag = compute $ A.zipWith magnitude' dX dY
    !orient = compute $ A.zipWith orientation dX dY
    magnitude' :: Pixel Y' Float -> Pixel Y' Float -> Float
    magnitude' (PixelY' x) (PixelY' y) = sqrt (x * x + y * y)
    {-# INLINE magnitude' #-}
    {-# INLINE orientation #-}
    orientation :: Pixel Y' Float -> Pixel Y' Float -> Word8
    orientation (PixelY' x) (PixelY' y)
         -- Don't bother computing orientation if vector is below threshold.
      | x >= negate threshLow
      , x < threshLow
      , y >= negate threshLow
      , y < threshLow = orientUndef
      | otherwise
                -- Determine the angle of the vector and rotate it around a bit
                -- to make the segments easier to classify.
       =
        let !d = atan2 y x
            !dRot = (d - (pi / 8)) * (4 / pi)
                -- Normalise angle to beween 0..8
            !dNorm =
              if dRot < 0
                then dRot + 8
                else dRot
                -- Doing explicit tests seems to be faster than using the FP floor function.
         in fromIntegral $
            I#
              (if dNorm >= 4
                 then if dNorm >= 6
                        then if dNorm >= 7
                               then 255# -- 7
                               else 192# -- 6
                        else if dNorm >= 5
                               then 128# -- 5
                               else 64# -- 4
                 else if dNorm >= 2
                        then if dNorm >= 3
                               then 255# -- 3
                               else 192# -- 2
                        else if dNorm >= 1
                               then 128# -- 1
                               else 64# -- 0
               )
{-# INLINE gradientMagOrient #-}


gradientMagOrient' ::
     Float
  -> Image S Y' Float
  -> IO (Array U Ix2 (Float, Word8))
gradientMagOrient' !threshLow !img =
  computeIO $
  mapStencil Edge (liftA2 (\ !x !y -> (magnitude' x y, orientation x y)) sobelX sobelY) img
  where
    magnitude' :: Pixel Y' Float -> Pixel Y' Float -> Float
    magnitude' (PixelY' x) (PixelY' y) = sqrt (x * x + y * y)
    {-# INLINE magnitude' #-}
    {-# INLINE orientation #-}
    orientation :: Pixel Y' Float -> Pixel Y' Float -> Word8
    orientation (PixelY' x) (PixelY' y)
         -- Don't bother computing orientation if vector is below threshold.
      | x >= negate threshLow
      , x < threshLow
      , y >= negate threshLow
      , y < threshLow = orientUndef
      | otherwise
                -- Determine the angle of the vector and rotate it around a bit
                -- to make the segments easier to classify.
       =
        let !d = atan2 y x
            !dRot = (d - (pi / 8)) * (4 / pi)
                -- Normalise angle to beween 0..8
            !dNorm =
              if dRot < 0
                then dRot + 8
                else dRot
                -- Doing explicit tests seems to be faster than using the FP floor function.
         in fromIntegral $
            I#
              (if dNorm >= 4
                 then if dNorm >= 6
                        then if dNorm >= 7
                               then 255# -- 7
                               else 192# -- 6
                        else if dNorm >= 5
                               then 128# -- 5
                               else 64# -- 4
                 else if dNorm >= 2
                        then if dNorm >= 3
                               then 255# -- 3
                               else 192# -- 2
                        else if dNorm >= 1
                               then 128# -- 1
                               else 64# -- 0
               )
{-# INLINE gradientMagOrient' #-}


suppress' :: Float -> Float -> (Array U Ix2 (Float, Word8)) -> IO (Image S Y' Word8)
suppress' !threshLow !threshHigh !dMagOrient =
  computeIO $ mapStencil (Fill (0, 0)) (makeUnsafeStencil 3 1 comparePts) dMagOrient
  where
    {-# INLINE comparePts #-}
    comparePts _ getMag
      | o == orientUndef = edgeNone
      | o == orientHoriz = isMax (getMag (0 :. -1)) (getMag (0 :. 1))
      | o == orientVert = isMax (getMag (-1 :. 0)) (getMag (1 :. 0))
      | o == orientNegDiag = isMax (getMag (-1 :. 1)) (getMag (1 :. -1))
      | o == orientPosDiag = isMax (getMag (-1 :. -1)) (getMag (1 :. 1))
         -- | o == orientNegDiag   = isMax (getMag (-1 :. -1)) (getMag ( 1 :.  1)) --?????
         -- | o == orientPosDiag   = isMax (getMag (-1 :.  1)) (getMag ( 1 :. -1)) --?????
      | otherwise = edgeNone
      where
        (!m, !o) = getMag (0 :. 0)
        {-# INLINE isMax #-}
        isMax intensity1 intensity2
          | m < threshLow = edgeNone
          | m < fst intensity1 = edgeNone
          | m < fst intensity2 = edgeNone
          | m < threshHigh = edgeWeak
          | otherwise = edgeStrong
{-# INLINE suppress' #-}

-- | Suppress pixels that are not local maxima, and use the magnitude to classify maxima
--   into strong and weak (potential) edges.
suppress :: Float -> Float -> (Array S Ix2 Float, Array S Ix2 Word8) -> IO (Image S Y' Word8)
suppress !threshLow !threshHigh (!dMag, !dOrient) =
  pure $ compute $ mapStencil (Fill 0) (makeUnsafeStencil 3 1 comparePts) dMag
  where
    {-# INLINE comparePts #-}
    comparePts !ix getMag
      | o == orientUndef = edgeNone
      | o == orientHoriz = isMax (getMag (0 :. -1)) (getMag (0 :. 1))
      | o == orientVert = isMax (getMag (-1 :. 0)) (getMag (1 :. 0))
      | o == orientNegDiag = isMax (getMag (-1 :. 1)) (getMag (1 :. -1))
      | o == orientPosDiag = isMax (getMag (-1 :. -1)) (getMag (1 :. 1))
         -- | o == orientNegDiag   = isMax (getMag (-1 :. -1)) (getMag ( 1 :.  1)) --?????
         -- | o == orientPosDiag   = isMax (getMag (-1 :.  1)) (getMag ( 1 :. -1)) --?????
      | otherwise = edgeNone
      where
        !o = unsafeIndex dOrient ix
        !m = getMag (0 :. 0)
        {-# INLINE isMax #-}
        isMax !intensity1 !intensity2
          | m < threshLow = edgeNone
          | m < intensity1 = edgeNone
          | m < intensity2 = edgeNone
          | m < threshHigh = edgeWeak
          | otherwise = edgeStrong
{-# INLINE suppress #-}


-- | Select indices of strong edges.
selectStrong :: Image S Y' Word8 -> IO (Array S Ix1 Ix1)
selectStrong =
  computeIO .
  simapMaybe
    (\ !ix !e ->
       if e == edgeStrong
         then Just ix
         else Nothing) .
  flatten
{-# INLINE selectStrong #-}


-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
wildfire ::
     Image S Y' Word8 -- ^ Image with strong and weak edges set.
  -> Array S Ix1 Ix1 -- ^ Array containing indices of strong edges.
  -> IO (Image S Y' Word8)
wildfire img vStrong = do
  vStrong' <- A.thaw vStrong
  -- Stack of image indices we still need to consider.
  vStack <- A.unsafeLinearGrow vStrong' (Sz lenImg)
  -- Burn in new edges.
  vImg <- A.new sz
  burn vImg vStack (unSz (size vStrong))
  unsafeFreeze (getComp img) vImg
  where
    !sz = A.size img
    !lenImg = totalElem sz
    burn ::
         MArray RealWorld S Ix2 (Pixel Y' Word8)
      -> MArray RealWorld S Ix1 Ix1
      -> Int
      -> IO ()
    burn !vImg !vStack = go
      where
        push !ix !t =
          case indexM img ix of
            Nothing -> pure t
            Just xSrc -> do
              xDst <- unsafeRead vImg ix
              if xDst == edgeNone && xSrc == edgeWeak
                -- If this ix is weak in the source then set it to strong in the result
                -- and push the ix onto the stack.
                then do
                  unsafeWrite vStack t $ toLinearIndex sz ix
                  return (t + 1)
                else return t
        {-# INLINE push #-}
        go !top
          | top == 0 = return ()
          | otherwise = do
            let !top' = top - 1
            ix@(y :. x) <- fromLinearIndex sz <$> readM vStack top'
            unsafeWrite vImg ix (edgeStrong)
            push (y - 1 :. x - 1) top' >>= push (y - 1 :. x) >>=
              push (y - 1 :. x + 1) >>=
              push (y :. x - 1) >>=
              push (y :. x + 1) >>=
              push (y + 1 :. x - 1) >>=
              push (y + 1 :. x) >>=
              push (y + 1 :. x + 1) >>=
              go
{-# INLINE wildfire #-}


--------------------------------------------
