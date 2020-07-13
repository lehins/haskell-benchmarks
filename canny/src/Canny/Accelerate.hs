{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-orphans #-}
-- This module defines the final phase of the Canny algorithm, a recursive
-- algorithm to "connect" pixels of the output image.
--
-- This function originally written by Ben Lippmeier for Repa.
--
-- NOTE: for best performance this needs to be compiled with the following GHC options:
--       -fllvm -optlo-O3 -Odph -fno-liberate-case
--       -funfolding-use-threshold100 -funfolding-keeness-factor100
module Canny.Accelerate where
-- Copy from https://github.com/AccelerateHS/accelerate-examples/tree/59c491b236c648548aaaaeb2a99239a44b3cedcb/examples/canny/src-acc

-- An implementation of the Canny edge detection algorithm
--
--   J. F. Canny, "A Computational Approach to Edge Detection" in _Pattern
--   Analysis and Machine Intelligence_, (6), 1986.
--
-- This module defines the first seven data-parallel sections of the algorithm.
-- The last phase, which uses a recursive algorithm to "connect" pixels that
-- form the output image, is defined in the module Wildfire and implemented
-- using Repa.
--

import qualified Prelude                                as P
import Prelude (IO, Functor(..), Monad(..), (>>=), seq, otherwise)

import Control.DeepSeq
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO.Codec.BMP               as A
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.IO.Data.Vector.Storable    as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
import Data.Array.Repa.Repr.Accelerate                  as A
import Data.Array.Repa.Repr.Unboxed                     ( U )
import qualified Data.Array.Repa                        as R
import qualified Data.Vector.Unboxed                    as V
import qualified Data.Vector.Unboxed.Mutable            as VM
import qualified Codec.Picture                          as JP
import qualified Data.Vector.Storable                   as VS

fromJuicyPixels :: JP.Image JP.PixelRGBA8 -> Image RGBA32
fromJuicyPixels (JP.Image w h vec) = fromVectors (Z :. h :. w) (VS.unsafeCast vec)

fromJuicyPixelsY :: JP.Image JP.PixelF -> Image Float
fromJuicyPixelsY (JP.Image w h vec) = fromVectors (Z :. h :. w) vec


runCanny ::
     Float -> Float -> Image RGBA32 -> IO (R.Array U R.DIM2 Word8)
runCanny (constant -> low) (constant -> high) img = do
  let image = CPU.runN
              ( nonMaximumSuppression low high
              . gradientMagDir low
              . gaussianY
              . gaussianX
              . toGreyscale
              ) img
      strong = CPU.runN selectStrong image
  -- let (image, strong) =
  --       CPU.run $ canny threshLow threshHigh (use img)
  wildfire (A.toRepa image) (A.toRepa strong)


-- Canny algorithm -------------------------------------------------------------

canny :: Float -> Float -> Acc (Image RGBA32) -> (Acc (Image Float), Acc (Vector Int))
canny (constant -> low) (constant -> high)
  = stage1
  . nonMaximumSuppression low high
  . gradientMagDir low
  . gaussianY
  . gaussianX
  . toGreyscale
  where
    stage1 x = (x, selectStrong x)

blur :: Image Float -> Image Float
blur img = CPU.runN $ gaussianY $ gaussianX $ use img
{-# INLINE blur #-}


-- Accelerate component --------------------------------------------------------

type Image a            = Array DIM2 a

type Stencil5x1 a       = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a       = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

-- Classification of the output pixel
data Orient     = Undef | PosD | Vert | NegD | Horiz
data Edge       = None  | Weak | Strong

instance Elt e => NFData (R.Array A R.DIM1 e) where
  rnf = (`R.deepSeqArray` ())
instance Elt e => NFData (R.Array A R.DIM2 e) where
  rnf = (`R.deepSeqArray` ())



orient :: Orient -> Int
orient Undef    = 0
orient PosD     = 64
orient Vert     = 128
orient NegD     = 192
orient Horiz    = 255

orient' :: Orient -> Exp Int
orient' = constant . orient

edge :: Edge -> Float
edge None       = 0
edge Weak       = 0.5
edge Strong     = 1.0

edge' :: Edge -> Exp Float
edge' = constant . edge

convolve5x1 :: Num a => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: Num a => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]


-- RGB to Greyscale conversion, in the range [0,255]
--
toGreyscale :: Acc (Image RGBA32) -> Acc (Image Float)
toGreyscale = map (\rgba -> 255 * luminance (unpackRGB rgba))


-- Separable Gaussian blur in the x- and y-directions
--
gaussianX :: Acc (Image Float) -> Acc (Image Float)
gaussianX = stencil (convolve5x1 gaussian) A.clamp
  where
    gaussian = P.map (/16) [ 1, 4, 6, 4, 1 ]

gaussianY :: Acc (Image Float) -> Acc (Image Float)
gaussianY = stencil (convolve1x5 gaussian) A.clamp
  where
    gaussian = P.map (/16) [ 1, 4, 6, 4, 1 ]


-- Gradients in the x- and y- directions
--
gradientX :: Acc (Image Float) -> Acc (Image Float)
gradientX = stencil grad A.clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((u, _, x)
         ,(v, _, y)
         ,(w, _, z)) = x + (2*y) + z - u - (2*v) - w

gradientY :: Acc (Image Float) -> Acc (Image Float)
gradientY = stencil grad A.clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((x, y, z)
         ,(_, _, _)
         ,(u, v, w)) = x + (2*y) + z - u - (2*v) - w


-- Classify the magnitude and orientation of the image gradient.
--
-- Because accelerate supports generalised stencil functions, not just
-- convolutions, we can combine the x- and y- sobel operators and save some
-- memory bandwidth.
--
gradientMagDir
    :: Exp Float
    -> Acc (Image Float)
    -> Acc (Array DIM2 (Float,Int))
gradientMagDir low = stencil magdir A.clamp
  where
    magdir :: Stencil3x3 Float -> Exp (Float,Int)
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
          -- Image gradients
          dx          = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
          dy          = v0 + (2*v1) + v2 - v5 - (2*v6) - v7

          -- Magnitude
          mag         = sqrt (dx * dx + dy * dy)

          -- Direction
          --
          -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          theta       = atan2 dy dx
          alpha       = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          norm        = alpha + 8 * fromIntegral (boolToInt (alpha <= 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          undef       = abs dx <= low && abs dy <= low
          dir         = boolToInt (not undef) * ((64 * (1 + floor norm `mod` 4)) `min` 255)
      in
      lift (mag, dir)


-- Non-maximum suppression classifies pixels that are the local maximum along
-- the direction of the image gradient as either strong or weak edges. All other
-- pixels are not considered edges at all.
--
-- The image intensity is in the range [0,1]
--
nonMaximumSuppression
  :: Exp Float
  -> Exp Float
  -> Acc (Image (Float,Int))
  -> Acc (Image Float)
nonMaximumSuppression low high magdir =
  generate (shape magdir) $ \ix ->
    let -- The input parameters
        --
        (mag, dir)      = unlift (magdir ! ix)
        Z :. h :. w     = unlift (shape magdir)
        Z :. y :. x     = unlift ix

        -- Determine the points that lie either side of this point along to the
        -- direction of the image gradient.
        --
        -- The direction coding:
        --
        --   192   128   64
        --          |
        --   255 --- ---
        --
        offsetx         = dir > orient' Vert  ? (-1, dir < orient' Vert ? (1, 0))
        offsety         = dir < orient' Horiz ? (-1, 0)

        (fwd, _)        = unlift $ magdir ! lift (limit (Z :. y+offsety :. x+offsetx)) :: (Exp Float, Exp Int)
        (rev, _)        = unlift $ magdir ! lift (limit (Z :. y-offsety :. x-offsetx)) :: (Exp Float, Exp Int)

        limit (Z:.u:.v) = Z :. 0 `max` u `min` (h-1) :. 0 `max` v `min` (w-1)

        -- Try to avoid doing explicit tests to avoid warp divergence.
        --
        none            = dir == orient' Undef || mag < low || mag < fwd || mag < rev
        strong          = mag >= high
    in
    fromIntegral (boolToInt (not none) * (1 + boolToInt strong)) * 0.5


-- Extract the linear indices of the strong edges
--
selectStrong
  :: Acc (Image Float)
  -> Acc (Array DIM1 Int)
selectStrong img =
  let strong            = map (\x -> boolToInt (x == edge' Strong)) (flatten img)
      (targetIdx, len)  = unlift (scanl' (+) 0 strong)
      indices           = enumFromN (index1 $ size img) 0
      zeros             = fill (index1 $ the len) 0
  in
  permute const zeros (\ix -> strong!ix == 0 ? (ignore, index1 $ targetIdx!ix)) indices


-- Repa component --------------------------------------------------------------

-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
--
wildfire
    :: R.Array A R.DIM2 Float           -- ^ Image with strong and weak edges set.
    -> R.Array A R.DIM1 Int             -- ^ Array containing flat indices of strong edges.
    -> IO (R.Array U R.DIM2 Word8)
{-# NOINLINE wildfire #-}
wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO
        return  $ sh `seq` vec `seq` R.fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        shImg           = R.extent img

        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong  <- R.toUnboxed `fmap` R.computeUnboxedP (R.delay arrStrong)
                vStrong' <- V.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)

                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- V.unsafeFreeze vImg
                return  (R.extent img, vImg')


        burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top P.== 0
         = return ()

         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (R.Z R.:. y R.:. x) = R.fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push ix t =
                      if R.inShape shImg ix
                         then pushWeak vImg vStack ix t
                         else return t

                VM.write vImg n 255
                 >>  push (R.Z R.:. y - 1 R.:. x - 1) top'
                 >>= push (R.Z R.:. y - 1 R.:. x    )
                 >>= push (R.Z R.:. y - 1 R.:. x + 1)

                 >>= push (R.Z R.:. y     R.:. x - 1)
                 >>= push (R.Z R.:. y     R.:. x + 1)

                 >>= push (R.Z R.:. y + 1 R.:. x - 1)
                 >>= push (R.Z R.:. y + 1 R.:. x    )
                 >>= push (R.Z R.:. y + 1 R.:. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = R.toIndex (R.extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst P.== 0 P.&& xSrc P.== edge Weak
                 then do
                        VM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                        return (top + 1)

                 else   return top


flipV :: R.Array R.U R.DIM2 Word8 -> R.Array R.U R.DIM2 Word8
flipV arr
  = R.computeUnboxedS
  $ R.backpermute sh p arr
  where
    sh@(R.Z R.:. rows R.:. _) = R.extent arr
    p (R.Z R.:. j R.:. i)     = R.Z R.:. rows-j-1 R.:. i
