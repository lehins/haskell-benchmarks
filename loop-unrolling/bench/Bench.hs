{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity (runIdentityT)
import Criterion.Main
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits
import Data.ByteString.Internal (ByteString (..))
import Data.ByteString.Short (ShortByteString (..), fromShort)
import GHC.Exts
import GHC.ForeignPtr
import GHC.ST (ST (..))
import GHC.Word
import System.Random.Stateful

st_ :: (State# s -> State# s) -> ST s ()
st_ m# = ST $ \s# -> (# m# s#, () #)
{-# INLINE st_ #-}

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray (I# n#) =
  ST $ \s# ->
    case newByteArray# n# s# of
      (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newMutableByteArray #-}

newPinnedMutableByteArray :: Int -> ST s (MutableByteArray s)
newPinnedMutableByteArray (I# n#) =
  ST $ \s# ->
    case newPinnedByteArray# n# s# of
      (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newPinnedMutableByteArray #-}

freezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
freezeMutableByteArray (MutableByteArray mba#) =
  ST $ \s# ->
    case unsafeFreezeByteArray# mba# s# of
      (# s'#, ba# #) -> (# s'#, ByteArray ba# #)

writeWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8 (MutableByteArray mba#) (I# i#) (W8# w#) = st_ (writeWord8Array# mba# i# w#)
{-# INLINE writeWord8 #-}

writeWord64LE_loop :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeWord64LE_loop mba fromByteIx = go fromByteIx
  where
    !toByteIx = fromByteIx + 8
    go !i !z =
      when (i < toByteIx) $ do
        writeWord8 mba i (fromIntegral z :: Word8)
        go (i + 1) (z `shiftR` 8)
{-# INLINE writeWord64LE_loop #-}

writeChecksWord64LE_unrolled :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeChecksWord64LE_unrolled mba i w64 = do
  let !toByteIx = i + 8
  when (i < toByteIx) $ do
    writeWord8 mba i (fromIntegral w64)
    let !i1 = i + 1
        w64_8 = w64 `shiftR` 8
    when (i1 < toByteIx) $ do
      writeWord8 mba i1 (fromIntegral w64_8)
      let !i2 = i1 + 1
          w64_16 = w64_8 `shiftR` 8
      when (i2 < toByteIx) $ do
        writeWord8 mba i2 (fromIntegral w64_16)
        let !i3 = i2 + 1
            w64_24 = w64_16 `shiftR` 8
        when (i3 < toByteIx) $ do
          writeWord8 mba i3 (fromIntegral w64_24)
          let !i4 = i3 + 1
              w64_32 = w64_24 `shiftR` 8
          when (i4 < toByteIx) $ do
            writeWord8 mba i4 (fromIntegral w64_32)
            let !i5 = i4 + 1
                w64_40 = w64_32 `shiftR` 8
            when (i5 < toByteIx) $ do
              writeWord8 mba i5 (fromIntegral w64_40)
              let !i6 = i5 + 1
                  w64_48 = w64_40 `shiftR` 8
              when (i6 < toByteIx) $ do
                writeWord8 mba i6 (fromIntegral w64_48)
                let !i7 = i6 + 1
                    w64_56 = w64_48 `shiftR` 8
                when (i7 < toByteIx) $ do
                  writeWord8 mba i7 (fromIntegral w64_56)
{-# INLINE writeChecksWord64LE_unrolled #-}

writeWord64LE_unrolled :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeWord64LE_unrolled mba i w64 = do
  writeWord8 mba i (fromIntegral w64)
  writeWord8 mba (i + 1) (fromIntegral (w64 `shiftR` 8))
  writeWord8 mba (i + 2) (fromIntegral (w64 `shiftR` 16))
  writeWord8 mba (i + 3) (fromIntegral (w64 `shiftR` 24))
  writeWord8 mba (i + 4) (fromIntegral (w64 `shiftR` 32))
  writeWord8 mba (i + 5) (fromIntegral (w64 `shiftR` 40))
  writeWord8 mba (i + 6) (fromIntegral (w64 `shiftR` 48))
  writeWord8 mba (i + 7) (fromIntegral (w64 `shiftR` 56))
{-# INLINE writeWord64LE_unrolled #-}

fillByteArray :: (forall s. MutableByteArray s -> Int -> Word64 -> ST s ()) -> Int -> ByteArray
fillByteArray fillWith = \n -> runST $ do
  let nBytes = n * 8
  mba <- newMutableByteArray nBytes
  let go !i !x
        | i < nBytes = do
            fillWith mba i x
            go (i + 8) (x - 1)
        | otherwise = pure ()
  go 0 (maxBound :: Word64)
  freezeMutableByteArray mba
{-# INLINE fillByteArray #-}

bench_writeWord64LE_loop :: Int -> ByteArray
bench_writeWord64LE_loop = fillByteArray writeWord64LE_loop
{-# NOINLINE bench_writeWord64LE_loop #-}

bench_writeWord64LE_unrolled :: Int -> ByteArray
bench_writeWord64LE_unrolled = fillByteArray writeWord64LE_unrolled
{-# NOINLINE bench_writeWord64LE_unrolled #-}

bench_writeChecksWord64LE_unrolled :: Int -> ByteArray
bench_writeChecksWord64LE_unrolled = fillByteArray writeChecksWord64LE_unrolled
{-# NOINLINE bench_writeChecksWord64LE_unrolled #-}

writeByteSliceWord64LE :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
writeByteSliceWord64LE mba fromByteIx toByteIx = go fromByteIx
  where
    go !i !z =
      when (i < toByteIx) $ do
        writeWord8 mba i (fromIntegral z :: Word8)
        go (i + 1) (z `shiftR` 8)
{-# INLINE writeByteSliceWord64LE #-}

writeByteSliceWord64LE_unrolled :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
writeByteSliceWord64LE_unrolled mba i toByteIx w64 = do
  when (i < toByteIx) $ do
    writeWord8 mba i (fromIntegral w64)
    let !i1 = i + 1
        w64_8 = w64 `shiftR` 8
    when (i1 < toByteIx) $ do
      writeWord8 mba i1 (fromIntegral w64_8)
      let !i2 = i1 + 1
          w64_16 = w64_8 `shiftR` 8
      when (i2 < toByteIx) $ do
        writeWord8 mba i2 (fromIntegral w64_16)
        let !i3 = i2 + 1
            w64_24 = w64_16 `shiftR` 8
        when (i3 < toByteIx) $ do
          writeWord8 mba i3 (fromIntegral w64_24)
          let !i4 = i3 + 1
              w64_32 = w64_24 `shiftR` 8
          when (i4 < toByteIx) $ do
            writeWord8 mba i4 (fromIntegral w64_32)
            let !i5 = i4 + 1
                w64_40 = w64_32 `shiftR` 8
            when (i5 < toByteIx) $ do
              writeWord8 mba i5 (fromIntegral w64_40)
              let !i6 = i5 + 1
                  w64_48 = w64_40 `shiftR` 8
              when (i6 < toByteIx) $ do
                writeWord8 mba i6 (fromIntegral w64_48)
                let !i7 = i6 + 1
                    w64_56 = w64_48 `shiftR` 8
                when (i7 < toByteIx) $ do
                  writeWord8 mba i7 (fromIntegral w64_56)
{-# INLINE writeByteSliceWord64LE_unrolled #-}

genByteArrayST :: Int -> ST s Word64 -> ST s ByteArray
genByteArrayST n0 action = do
  let !n = max 0 n0
  mba <- newMutableByteArray n
  runIdentityT $ defaultUnsafeUniformFillMutableByteArrayT mba n (lift action)
  freezeMutableByteArray mba
{-# INLINE genByteArrayST #-}

uniformByteArray_local
  :: RandomGen g
  => Bool
  -> Int
  -> g
  -> (ByteArray, g)
uniformByteArray_local isPinned n0 g =
  runST $ do
    let !n = max 0 n0
    mba <-
      if isPinned
        then newPinnedMutableByteArray n
        else newMutableByteArray n
    g' <- defaultUnsafeUniformFillMutableByteArray mba n g
    ba <- freezeMutableByteArray mba
    pure (ba, g')
{-# INLINE uniformByteArray_local #-}

uniformByteArrayM_local
  :: (RandomGen g, Monad m) => Bool -> Int -> StateGenM g -> StateT g m ByteArray
uniformByteArrayM_local isPinned n _ =
  state (uniformByteArray_local isPinned n)

uniformShortByteString_local :: Int -> StdGen -> (ShortByteString, StdGen)
uniformShortByteString_local n g =
  case uniformByteArray_local False n g of
    (ByteArray ba#, g') -> (SBS ba#, g')
{-# INLINE uniformShortByteString_local #-}

defaultUnsafeUniformFillMutableByteArray
  :: RandomGen g
  => MutableByteArray s
  -> Int
  -- ^ Number of random bytes to write into the array
  -> g
  -- ^ ST action that can generate 8 random bytes at a time
  -> ST s g
defaultUnsafeUniformFillMutableByteArray mba n g =
  flip execStateT g $
    defaultUnsafeUniformFillMutableByteArrayT mba n (state genWord64)
{-# INLINE defaultUnsafeUniformFillMutableByteArray #-}

defaultUnsafeUniformFillMutableByteArrayT
  :: (Monad (t (ST s)), MonadTrans t)
  => MutableByteArray s
  -> Int
  -> t (ST s) Word64
  -> t (ST s) ()
defaultUnsafeUniformFillMutableByteArrayT mba n gen64 = do
  let !n64 = n `quot` 8
      -- !endIx64 = offset + n64 * 8
      !nrem = n `rem` 8
  let go !i =
        when (i < n64) $ do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          lift $ writeWord64LE mba i w64
          go (i + 1)
  go 0
  when (nrem > 0) $ do
    let !endIx = n
    w64 <- gen64
    -- In order to not mess up the byte order we write 1 byte at a time in
    -- Little endian order. It is tempting to simply generate as many bytes as we
    -- still need using smaller generators (eg. uniformWord8), but that would
    -- result in inconsistent tail when total length is slightly varied.
    lift $ writeByteSliceWord64LE mba (endIx - nrem) endIx w64
{-# INLINE defaultUnsafeUniformFillMutableByteArrayT #-}

writeWord64LE :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeWord64LE (MutableByteArray mba#) (I# i#) (W64# w64#) =
  st_ (writeWord64Array# mba# i# w64#)
-- st_ (writeWord8ArrayAsWord64# mba# i# w64#)
{-# INLINE writeWord64LE #-}

genByteArrays
  :: StdGen
  -> [Int]
  -> ([ByteArray], StdGen)
genByteArrays g0 =
  foldr (\k (acc, g) -> let !(!ba, !g') = uniformByteArray_local False k g in (ba : acc, g')) ([], g0)
{-# NOINLINE genByteArrays #-}

genShortByteStrings
  :: StdGen
  -> [Int]
  -> ([ShortByteString], StdGen)
genShortByteStrings g0 =
  foldr (\k (acc, g) -> let !(!ba, !g') = genShortByteString k g in (ba : acc, g')) ([], g0)
{-# NOINLINE genShortByteStrings #-}

genShortByteStringsOriginal
  :: StdGen
  -> [Int]
  -> ([ShortByteString], StdGen)
genShortByteStringsOriginal gen =
  \ks -> runStateGen gen $ \g -> mapM (`uniformShortByteString` g) ks
{-# NOINLINE genShortByteStringsOriginal #-}

byteArrayToShortByteString :: ByteArray -> ShortByteString
byteArrayToShortByteString (ByteArray ba#) = SBS ba#
{-# INLINE byteArrayToShortByteString #-}

genByteString_local :: RandomGen g => Int -> g -> (ByteString, g)
genByteString_local n g = runStateGenST g (uniformByteStringM_local n)
{-# INLINE genByteString_local #-}

pinnedByteArrayToByteString :: ByteArray# -> ByteString
pinnedByteArrayToByteString ba# =
  PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}

uniformByteStringM_local :: (RandomGen g, Monad m) => Int -> StateGenM g -> StateT g m ByteString
uniformByteStringM_local n g = do
  shortByteStringToByteString . byteArrayToShortByteString
    <$> uniformByteArrayM_local True n g
-- ba <- (state (uniformByteArray_local True n))
-- pure $
--   if isTrue# (isByteArrayPinned# ba#)
--     then pinnedByteArrayToByteString ba#
--     else fromShort (SBS ba#)
{-# INLINE uniformByteStringM_local #-}

shortByteStringToByteString :: ShortByteString -> ByteString
shortByteStringToByteString ba =
  let !(SBS ba#) = ba
   in if isTrue# (isByteArrayPinned# ba#)
        then pinnedByteArrayToByteString ba#
        else fromShort ba
{-# INLINE shortByteStringToByteString #-}

main :: IO ()
main = do
  genLengths :: ([Int], StdGen) <-
    -- create 50000 small lengths that are needed for ShortByteString generation
    runStateGenT (mkStdGen 2020) $ \g -> replicateM 50000 (uniformRM (16 + 1, 16 + 7) g)
  let !n = 1024 * 1024
      !n100Mb = 100 * 1024 * 1024
  defaultMain
    [ bgroup
        "NOINLINE"
        [ bench "writeWord64LE_loop" $ nf bench_writeWord64LE_loop n
        , bench "writeWord64LE_unrolled" $ nf bench_writeWord64LE_unrolled n
        , bench "writeChecksWord64LE_unrolled" $ nf bench_writeChecksWord64LE_unrolled n
        ]
    , bgroup
        "INLINE"
        [ bench "writeWord64LE_loop" $ nf (fillByteArray writeWord64LE_loop) n
        , bench "writeWord64LE_unrolled" $ nf (fillByteArray writeWord64LE_unrolled) n
        , bench "writeChecksWord64LE_unrolled" $
            nf (fillByteArray writeChecksWord64LE_unrolled) n
        ]
    , bgroup
        "Bytes"
        [ env (pure genLengths) $ \ ~(ns, gen) ->
            bench "uniformShortByteString" $ nf (genShortByteStringsOriginal gen) ns
        , env (pure genLengths) $ \ ~(ns, gen) ->
            bench "genShortByteStrings" $ nf (genShortByteStrings gen) ns
        , env (pure genLengths) $ \ ~(ns, gen) ->
            bench "genByteArrays" $ nf (genByteArrays gen) ns
        , env (pure genLengths) $ \ ~(ns, gen) ->
            bench "uniformByteArray_local" $
              nf
                ( \ks -> runStateGen gen $
                    \_ -> mapM (\k -> state (uniformByteArray_local False k)) ks
                )
                ns
        ]
    , bgroup
        "ManyBytes"
        [ env getStdGen $
            bench "uniformByteArray_local 100MB" . nf (uniformByteArray_local False n100Mb)
        , env getStdGen $
            bench "genByteString 100MB" . nf (genByteString n100Mb)
        , env getStdGen $
            bench "genByteString_local 100MB" . nf (genByteString_local n100Mb)
        , env getStdGen $
          bench "genShortByteString 100MB" . nf (genShortByteString n100Mb)
        , env getStdGen $ \gen ->
            bench "uniformShortByteStringM 100MB" $
              nf (runStateGen gen . uniformShortByteString) n100Mb
        ]
    ]
