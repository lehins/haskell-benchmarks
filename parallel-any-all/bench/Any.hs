{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Control.Parallel.Strategies
import Control.Scheduler
import Control.Scheduler.Global
import Criterion.Main
import Data.Foldable as F
import Data.Functor.Identity
import Data.IORef
import Data.Int
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import qualified Data.Vector.Primitive as VP
import Prelude
import System.Mem

loopMaybeM :: Monad m => b -> (b -> Bool) -> (b -> b) -> a -> (b -> a -> m (Maybe a)) -> m a
loopMaybeM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc
      | condition step = f step acc >>= \case
          Just acc' -> go (increment step) acc'
          Nothing   -> pure acc
      | otherwise = pure acc
{-# INLINE loopMaybeM #-}


-- loopMaybeM :: Monad m => b -> (b -> Maybe b) -> a -> (b -> a -> m (Maybe a)) -> m a
-- loopMaybeM !startAt getNext !initAcc f = go startAt initAcc
--   where
--     go !step !acc =
--       case getNext step of
--         Nothing -> pure acc
--         Just next ->
--           f step acc >>= \case
--             Just acc' -> go next acc'
--             Nothing -> pure acc
-- {-# INLINE loopMaybeM #-}


loopShortM :: Monad m => Int -> (Int -> a -> m Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc = do
      shouldContinue <- condition step acc
      if shouldContinue
        then f step acc >>= go (increment step)
        else return acc
{-# INLINE loopShortM #-}

-- loopShortM :: Monad m => Int -> (Int -> a -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
-- loopShortM !startAt condition increment !initAcc f = go startAt initAcc
--   where
--     go !step !acc
--       | condition step acc = f step acc >>= go (increment step)
--       | otherwise = return acc
-- {-# INLINE loopShortM #-}



-- loopShortM ::
--      Monad m
--   => Int
--   -> (Int -> Bool)
--   -> (Int -> Int)
--   -> Int
--   -> m ()
--   -> a
--   -> (Int -> a -> m (a, Bool))
--   -> m a
-- loopShortM !init' condition increment cp cpAction !initAcc f = go 0 init' initAcc True
--   where
--     go !i !step !acc !cont
--       | cont && condition step = do
--           -- i' <- if i == cp then cpAction >> pure 0 else pure (i + 1)
--           -- f step acc >>= uncurry (go i' (increment step))
--           --i' <- if i == cp then cpAction >> pure 0 else pure (i + 1)
--           cpAction
--           f step acc >>= uncurry (go i (increment step))
--           -- (acc', cont') <- f step acc
--           -- go i' (increment step) acc' cont'
--       | otherwise = return acc
-- {-# INLINE loopShortM #-}


-- iterShortLinearM ::
--      (Index ix, Monad m)
--   => Sz ix -- ^ Size
--   -> Int -- ^ Linear start (must be non-negative)
--   -> Int -- ^ Linear end (must be less than or equal to @`totalElem` sz@)
--   -> Int -- ^ Increment (must not be zero)
--   -> Int
--   -> m ()
--   -> (Int -> Int -> Bool) -- ^ Continuation condition (continue if @True@)
--   -> a -- ^ Accumulator
--   -> (Int -> ix -> a -> m (a, Bool))
--   -> m a
-- iterShortLinearM !sz !k0 !k1 !inc !cp cpAction cond !acc f =
--   loopShortM k0 (`cond` k1) (+ inc) cp cpAction acc $ \ !i !acc0 -> f i (fromLinearIndex sz i) acc0
-- {-# INLINE iterShortLinearM #-}

anyS :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
anyS f arr = go 0
  where
    !k = elemsCount arr
    go !i
      | i < k = f (unsafeLinearIndex arr i) || go (i + 1)
      | otherwise = False
{-# INLINE anyS #-}


anySu :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
anySu f arr = go 0
  where
    !k = elemsCount arr
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 =
        f (unsafeLinearIndex arr i      ) ||
        f (unsafeLinearIndex arr (i + 1)) ||
        f (unsafeLinearIndex arr (i + 2)) ||
        f (unsafeLinearIndex arr (i + 3)) ||
        go (i + 4)
      | i < k = f (unsafeLinearIndex arr i) || go (i + 1)
      | otherwise = False
{-# INLINE anySu #-}



allSu :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
allSu f arr = go 0
  where
    !k = elemsCount arr
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 =
        f (unsafeLinearIndex arr i      ) &&
        f (unsafeLinearIndex arr (i + 1)) &&
        f (unsafeLinearIndex arr (i + 2)) &&
        f (unsafeLinearIndex arr (i + 3)) &&
        go (i + 4)
      | i < k = f (unsafeLinearIndex arr i) && go (i + 1)
      | otherwise = True
{-# INLINE allSu #-}


sur ::
     Source r ix a
  => (Bool -> Bool)
  -> (Bool -> Bool -> Bool)
  -> (a -> Bool)
  -> Array r ix a
  -> Bool
sur mneg g f arr = go 0
  where
    !k = elemsCount arr
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 =
        f (unsafeLinearIndex arr i      ) `g`
        f (unsafeLinearIndex arr (i + 1)) `g`
        f (unsafeLinearIndex arr (i + 2)) `g`
        f (unsafeLinearIndex arr (i + 3)) `g`
        go (i + 4)
      | i < k = f (unsafeLinearIndex arr i) `g` go (i + 1)
      | otherwise = mneg False
{-# INLINE sur #-}


foldlShort ::
     Source r ix e => (a -> Bool) -> (a -> e -> a) -> a -> Array r ix e -> a
foldlShort g f = ifoldlShort g (\acc _ -> f acc)
{-# INLINE foldlShort #-}

ifoldlShort ::
     Source r ix e => (a -> Bool) -> (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlShort g f initAcc arr =
  runIdentity $ loopShortM 0 (\i a -> pure (g a && i < k)) (+ 1) initAcc $ \ !i !acc ->
    pure $! f acc (fromLinearIndex sz i) (unsafeLinearIndex arr i)
  where
    !sz = size arr
    !k = totalElem sz
{-# INLINE ifoldlShort #-}

anySl :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
anySl f = foldlShort not (\acc e -> acc || f e) False
{-# INLINE anySl #-}

ifoldrShort ::
     Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrShort f initAcc arr = go 0
  where
    !sz = size arr
    !k = totalElem sz
    go !i
      | i < k = f (fromLinearIndex sz i) (unsafeLinearIndex arr i) (go (i + 1))
      | otherwise = initAcc
{-# INLINE ifoldrShort #-}

foldrShort ::
     Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrShort f = ifoldrShort (const f)
{-# INLINE foldrShort #-}

ifoldrSliceUnroll ::
     Source r ix e => Ix1 -> Sz Ix1 -> (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrSliceUnroll x0 (Sz k) f initAcc arr = go x0
  where
    !sz = size arr
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 =
        let !i1 = i + 1
            !i2 = i + 2
            !i3 = i + 3
        in f (fromLinearIndex sz i ) (unsafeLinearIndex arr i )
           (f (fromLinearIndex sz i1) (unsafeLinearIndex arr i1)
            (f (fromLinearIndex sz i2) (unsafeLinearIndex arr i2)
             (f (fromLinearIndex sz i3) (unsafeLinearIndex arr i3) (go (i + 4)))))
      | i < k = f (fromLinearIndex sz i) (unsafeLinearIndex arr i) (go (i + 1))
      | otherwise = initAcc
{-# INLINE ifoldrSliceUnroll #-}

ifoldrUnroll ::
     Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrUnroll f initAcc arr = ifoldrSliceUnroll 0 (Sz (elemsCount arr)) f initAcc arr
{-# INLINE ifoldrUnroll #-}

foldrUnroll ::
     Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrUnroll f = ifoldrUnroll (const f)
{-# INLINE foldrUnroll #-}

anySur :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
anySur f = foldrUnroll (\e acc -> f e || acc) False
{-# INLINE anySur #-}

anySr :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
anySr f = foldrShort (\e acc -> f e || acc) False
{-# INLINE anySr #-}

anySurSlice :: Source r ix a => Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> Bool
anySurSlice ix sz f = ifoldrSliceUnroll ix sz (\_ e acc -> f e || acc) False
{-# INLINE anySurSlice #-}


-- anySuM :: Source r ix a => Scheduler IO Bool -> Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> IO Bool
-- anySuM scheduler ix0 (Sz k) f arr = do
--   batchId <- getCurrentBatchId scheduler
--   let !k4 = k - (k `rem` 4)
--       go !i
--         | i < k4 = do
--           let r =
--                 f (unsafeLinearIndex arr i) ||
--                 f (unsafeLinearIndex arr (i + 1)) ||
--                 f (unsafeLinearIndex arr (i + 2)) ||
--                 f (unsafeLinearIndex arr (i + 3))
--           done <- hasBatchFinished scheduler batchId
--           if done || r
--             then r <$ cancelBatch scheduler True
--             else go (i + 4)
--         | i < k = do
--           let r = f (unsafeLinearIndex arr i)
--           done <- hasBatchFinished scheduler batchId
--           if done || r
--             then r <$ cancelBatch scheduler True
--             else go (i + 1)
--         | otherwise = pure False
--   go ix0
-- {-# INLINE anySuM #-}


-- anySuM ::
--      Source r ix a
--   => Scheduler IO ()
--   -> IORef Bool
--   -> BatchId
--   -> Ix1
--   -> Sz1
--   -> (a -> Bool)
--   -> Array r ix a
--   -> IO ()
-- anySuM scheduler ref batchId ix0 (Sz k) f arr = do
--   let go !i =
--         when (i < k) $ do
--           let r = f (unsafeLinearIndex arr i)
--           done <- hasBatchFinished scheduler batchId
--           if done || r
--             then do
--               writeIORef ref True
--               cancelBatch_ scheduler
--             else go (i + 1)
--   go ix0
-- {-# INLINE anySuM #-}


-- anyPur :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyPur f arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--   ref <- newIORef False
--   withScheduler_ (getComp arr) $ \scheduler -> do
--     batchId <- getCurrentBatchId scheduler
--     splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--       loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--         scheduleWork_ scheduler $ do
--           anySuM scheduler ref batchId start (Sz (start + chunkLength)) f arr
--       when (slackStart < totalLength) $
--         scheduleWork_ scheduler $ do
--           anySuM scheduler ref batchId slackStart (Sz totalLength) f arr
--   readIORef ref
-- {-# INLINE anyPur #-}


-- anySuM :: Source r ix a => Scheduler IO Bool -> BatchId -> Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> IO Bool
-- anySuM scheduler batchId ix0 (Sz k) f arr = do
--   let go !i
--         | i < k = do
--           done <- hasBatchFinished scheduler batchId
--           if done || f (unsafeLinearIndex arr i)
--             then cancelBatchWith scheduler batchId True
--             else go (i + 1)
--         | otherwise = pure False
--   go ix0
-- {-# INLINE anySuM #-}


anySuM :: Source r ix a => Batch IO Bool -> Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> IO Bool
anySuM batch ix0 (Sz k) f arr = go ix0
  where
    !k' = k - ix0
    !k4 = ix0 + (k' - (k' `rem` 4))
    go !i
      | i < k4 = do
        let r =
              f (unsafeLinearIndex arr i) ||
              f (unsafeLinearIndex arr (i + 1)) ||
              f (unsafeLinearIndex arr (i + 2)) ||
              f (unsafeLinearIndex arr (i + 3))
         in if r
              then cancelBatchWith batch True
              else do
                done <- hasBatchFinished batch
                if done
                  then pure True
                  else go (i + 4)
        -- done <- hasBatchFinished scheduler batchId
        -- if done ||
        --    f (unsafeLinearIndex arr i) ||
        --    f (unsafeLinearIndex arr (i + 1)) ||
        --    f (unsafeLinearIndex arr (i + 2)) ||
        --    f (unsafeLinearIndex arr (i + 3))
        --   then cancelBatchWith scheduler batchId True
        --   else go (i + 4)
      | i < k =
        if f (unsafeLinearIndex arr i)
          then cancelBatchWith batch True
          else go (i + 1)
      | otherwise = pure False
{-# INLINE anySuM #-}



allSuM :: Source r ix a => Batch IO Bool -> Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> IO Bool
allSuM batch ix0 (Sz k) f arr = go ix0
  where
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 = do
        let r = f (unsafeLinearIndex arr i) &&
                f (unsafeLinearIndex arr (i + 1)) &&
                f (unsafeLinearIndex arr (i + 2)) &&
                f (unsafeLinearIndex arr (i + 3))
         in if not r
              then cancelBatchWith batch False
              else do
                done <- hasBatchFinished batch
                if done
                  then pure False
                  else go (i + 4)

        -- done <- hasBatchFinished scheduler batchId
        -- if done ||
        --    f (unsafeLinearIndex arr i) ||
        --    f (unsafeLinearIndex arr (i + 1)) ||
        --    f (unsafeLinearIndex arr (i + 2)) ||
        --    f (unsafeLinearIndex arr (i + 3))
        --   then cancelBatchWith scheduler batchId True
        --   else go (i + 4)
      | i < k =
        if f (unsafeLinearIndex arr i)
          then cancelBatchWith batch True
          else go (i + 1)
      | otherwise = pure True
{-# INLINE allSuM #-}



suM ::
     Source r ix a
  => Batch IO Bool
  -> Ix1
  -> Sz1
  -> (Bool -> Bool)
  -> (Bool -> Bool -> Bool)
  -> (a -> Bool)
  -> Array r ix a
  -> IO Bool
suM batch ix0 (Sz k) mneg g f arr = go ix0
  where
    !k4 = k - (k `rem` 4)
    go !i
      | i < k4 = do
        let r = f (unsafeLinearIndex arr  i     ) `g`
                f (unsafeLinearIndex arr (i + 1)) `g`
                f (unsafeLinearIndex arr (i + 2)) `g`
                f (unsafeLinearIndex arr (i + 3))
         in if mneg r
              then cancelBatchWith batch (mneg True)
              else do
                done <- hasBatchFinished batch
                if done
                  then pure (mneg True)
                  else go (i + 4)
      | i < k =
        if mneg (f (unsafeLinearIndex arr i))
          then cancelBatchWith batch (mneg True)
          else go (i + 1)
      | otherwise = pure (mneg False)
{-# INLINE suM #-}


pur ::
     Source r ix e
  => (Bool -> Bool)
  -> (Bool -> Bool -> Bool)
  -> (e -> Bool)
  -> Array r ix e
  -> IO Bool
pur mneg g f arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  result <-
    withSchedulerR (getComp arr) $ \scheduler -> do
      batch <- getCurrentBatch scheduler
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            suM batch start (Sz (start + chunkLength)) mneg g f arr
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            suM batch slackStart (Sz totalLength) mneg g f arr
  pure $!
    case result of
      FinishedEarlyWith r -> r
      _ -> mneg False
{-# INLINE pur #-}



-- foldlShortUnroll :: Source r ix a =>
--   Scheduler IO Bool -> BatchId -> Ix1 -> Sz1 -> (a -> Bool) -> Array r ix a -> IO Bool
-- foldlShortUnroll scheduler batchId ix0 (Sz k) f arr = go ix0
--   where
--     !k4 = k - (k `rem` 4)
--     go !i
--       | i < k4 = do
--           let r = f (unsafeLinearIndex arr  i     ) `g`
--                   f (unsafeLinearIndex arr (i + 1)) `g`
--                   f (unsafeLinearIndex arr (i + 2)) `g`
--                   f (unsafeLinearIndex arr (i + 3))
--           -- then cancelBatchWith scheduler batchId True
--           -- else go (i + 4)
--          in if r
--               then cancelBatchWith scheduler batchId True
--               else do
--                 done <- hasBatchFinished scheduler batchId
--                 if done
--                   then pure True
--                   else go (i + 4)
--       | i < k =
--         if f (unsafeLinearIndex arr i)
--           then cancelBatchWith scheduler batchId True
--           else go (i + 1)
--       | otherwise = pure False
-- {-# INLINE foldlShortUnroll #-}



-- pur :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- pur scheduler f arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--   batchId <- getCurrentBatchId scheduler
--   splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--     loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--       scheduleWork scheduler $ do
--         anySuM scheduler batchId start (Sz (start + chunkLength)) f arr
--     when (slackStart < totalLength) $
--       scheduleWork scheduler $ do
--         anySuM scheduler batchId slackStart (Sz totalLength) f arr
--   results <- waitForCurrentBatch scheduler
--   pure $ F.foldl' (||) False results
-- {-# INLINE pur #-}

anyPur' :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyPur' = pur id (||)

anyPur :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyPur f arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  results <-
    withScheduler (getComp arr) $ \scheduler -> do
      batch <- getCurrentBatch scheduler
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            anySuM batch start (Sz (start + chunkLength)) f arr
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            anySuM batch slackStart (Sz totalLength) f arr
  pure $ F.foldl' (||) False results
{-# INLINE anyPur #-}


-- -- fast, but no short:
-- anyPur :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyPur f arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--   results <-
--     withScheduler (getComp arr) $ \scheduler -> do
--       splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--         loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--           scheduleWork scheduler $ do
--             pure $! ifoldrSliceUnroll start (Sz (start + chunkLength)) (\_ e a -> f e || a) False arr
--         when (slackStart < totalLength) $
--           scheduleWork scheduler $ do
--             pure $! ifoldrSliceUnroll slackStart (Sz totalLength) (\_ e a -> f e || a) False arr
--   pure $ F.foldl' (||) False results
-- {-# INLINE anyPur #-}




anyPi :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyPi f arr = do
  -- | comp == Seq = pure $ anyS f arr
  -- | otherwise = do
    ref <- newIORef False
    withScheduler_ comp $ \scheduler -> do
      batch <- getCurrentBatch scheduler
      iforSchedulerM_ scheduler arr $ \_ e -> do
        done <- hasBatchFinished batch
        when (done || f e) $ do
          atomicWriteIORef ref True
          terminate_ scheduler
    readIORef ref
  where
    !comp = getComp arr
{-# INLINE anyPi #-}

anyPig :: Source r ix e => Scheduler IO () -> (e -> Bool) -> Array r ix e -> IO Bool
anyPig scheduler f arr = do
  -- | comp == Seq = pure $ anyS f arr
  -- | otherwise = do
    ref <- newIORef False
    runBatch_ scheduler $ \batch -> do
      iforSchedulerM_ scheduler arr $ \_ e -> do
        done <- hasBatchFinished batch
        when (done || f e) $ do
          atomicWriteIORef ref True
          void $ cancelBatch_ batch
    readIORef ref
{-# INLINE anyPig #-}


-- anyPi :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyPi f arr
--   | comp == Seq = pure $ anyS f arr
--   | otherwise = do
--     results <-
--       withScheduler comp $ \scheduler -> do
--         batchId <- getCurrentBatchId scheduler
--         splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--           loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--             scheduleWork scheduler $ do
--               loopShortM
--                 start
--                 (\i a ->
--                    hasBatchFinished scheduler batchId >>= \done ->
--                      pure (not (done || a || i >= start + chunkLength)))
--                 (+ 1)
--                 False $ \ !i !acc ->
--                 if acc || f (unsafeLinearIndex arr i)
--                   then terminateWith scheduler True
--                   else pure False
--               --               pure (acc || f (unsafeLinearIndex arr i))
--               -- if res
--               --   then terminateWith scheduler True
--               --   else pure res
--               -- res <-
--               --   loopShortM
--               --     start
--               --     (\i a ->
--               --        hasBatchFinished scheduler batchId >>= \done ->
--               --          pure (not (done || a || i >= start + chunkLength)))
--               --     (+ 1)
--               --     False $ \ !i !acc -> pure (acc || f (unsafeLinearIndex arr i))
--               -- if res
--               --   then terminateWith scheduler True
--               --   else pure res
--           when (slackStart < totalLength) $
--             scheduleWork scheduler $ do
--               loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
--                 when (f (unsafeLinearIndex arr i)) $
--                 void $ terminateWith scheduler True
--               pure False
--     pure $ F.foldl' (||) False results
--   where
--     !comp = getComp arr
--     sz = size arr
--     totalLength = totalElem sz
-- {-# INLINE anyPi #-}


-- foldlShort ::
--      Source r ix e => (a -> Bool) -> (a -> e -> a) -> a -> Array r ix e -> a
-- foldlShort g f initAcc arr = runIdentity $
--   loopMaybeM 0 (< k) (+ 1) initAcc $ \ i !acc ->
--     pure $
--     if g acc
--       then Just $ f acc (unsafeLinearIndex arr i)
--       else Nothing
--   where
--     !k = elemsCount arr
-- {-# INLINE foldlShort #-}

-- anyP :: (Source r Ix1 e, Source r ix e) => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP f = splitReduce (\s arr -> pure $ anyS f arr) (\r a -> pure (a || r)) False

-- ifoldlShort ::
--      (Monad m, Source r ix e)
--   => (a -> Bool)
--   -> (a -> ix -> e -> m a)
--   -> a
--   -> Array r ix e
--   -> m a
-- ifoldlShort g f initAcc arr =
--   loopMaybeM 0 (< k) (+ 1) initAcc $ \i acc ->
--     if g acc
--       then Just <$> f acc (fromLinearIndex sz i) (unsafeLinearIndex arr i)
--       else pure Nothing
--   where
--     sz = size arr
--     k = totalElem sz
-- {-# INLINE ifoldlShort #-}


anyP :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyP f arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  results <-
    withScheduler (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            res <-
              loopShortM
                start
                (\i a -> pure (not a && i < start + chunkLength))
                (+ 1)
                False $ \ !i acc -> pure (acc || f (unsafeLinearIndex arr i))
            if res
              then terminateWith scheduler True
              else pure res
            -- loopM_ start (< start + chunkLength) (+ 1) $ \ !i ->
            --   when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
            -- pure False
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
              when (f (unsafeLinearIndex arr i)) $
              void $ terminateWith scheduler True
            pure False
  pure $ F.foldl' (||) False results
{-# INLINE anyP #-}


arrayNone :: Array P Ix1 Int
arrayNone = computeAs P $ A.enumFromStepN Seq 1 2 8000000
{-# NOINLINE arrayNone #-}

-- arrayLate :: Array P Ix1 Int
-- arrayLate =
--   computeAs P $
--   concat' 1 [A.enumFromStepN Seq 1 2 2999999, A.singleton 2, A.enumFromStepN Seq 1 2 1000000]
arrayLate :: Array P Ix1 Int
arrayLate =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 4000000, A.singleton 2, A.enumFromStepN Seq 1 2 3999999]
{-# NOINLINE arrayLate #-}


arrayLateShort :: Array P Ix1 Int
arrayLateShort =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 7000, A.singleton 2, A.enumFromStepN Seq 1 2 999]
{-# NOINLINE arrayLateShort #-}

arrayEarly :: Array P Ix1 Int
arrayEarly =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 1000000, A.singleton 2, A.enumFromStepN Seq 1 2 6999999]
{-# NOINLINE arrayEarly #-}

-- arrayEarly :: Array P Ix1 Int
-- arrayEarly =
--   computeAs P $
--   concat' 1 [A.singleton 2, A.enumFromStepN Seq 1 2 7000000, A.enumFromStepN Seq 1 2 999999]


anyBench :: Scheduler IO () -> String -> Vector P Int -> Benchmark
anyBench _scheduler name arr =
  bgroup
    name
    [ env (pure (A.toVector arr, A.toList arr)) $ \ ~(vecVP, _ls) ->
        bgroup
          "common"
          [ -- bench "list" $ whnf (F.any even) ls
          -- ,
            bench "vector" $ whnf (VP.any even) vecVP
          ]
    , bgroup
        "Massiv"
        [ --bench "anyS" $ whnf (anyS even . delay) arr
        -- , bench "anySl (foldlShort)" $ whnf (anySl even . delay) arr
        -- , bench "anySr (foldrShort)" $ whnf (anySr even . delay) arr
          -- bench "anySur (foldrUnroll)" $ whnf (anySur even . delay) arr
        -- , 
          bench "anySu" $ whnf (anySu even . delay) arr
        -- , bench "anyPi" $ whnfIO (anyPi even (setComp Par arr))
        --, bench "anyPig" $ whnfIO (anyPig scheduler even (setComp Par arr))
        , bench "massiv anyPur" $ whnfIO (anyPur even (setComp Par arr))
        , bench "massiv anyPur'" $ whnfIO (anyPur' even (setComp Par arr))
        -- , bench "anySu (D)" $ whnf (anySu even . delay) arr
        -- , bench "anySu (M)" $ whnf (anySu even . toManifest) arr
        -- , bench "F.any (D)" $ whnf (F.any even . delay) arr
        -- , bench "F.any (DS)" $ whnf (F.any even . toStreamArray) arr
        -- , bench "sany (DS)" $ whnf (sany even . toStreamArray) arr
        -- , bench "F.any (M)" $ whnfIO (pure $ F.any even (toManifest vec))
        -- , bench "massiv (sany)" $ whnfIO (pure $ A.any even (setComp Seq vec))
       --  , bench "anyP" $ whnfIO (anyP even (setComp Par vec))
       --  , bench "anySi" $ whnfIO (anyPi even (setComp Seq vec))
       -- -- , bench "anyS'" $ whnfIO (anyP' even (setComp Seq vec)) -- TOO SLOW
       --  , bench "anyP'" $ whnfIO (anyP' even (setComp Par vec))
        -- , bench "massiv (any Seq)" $ whnfIO (pure $ A.any even (setComp Seq vec))
        , bench "massiv (any Par)" $ whnf (A.any even . delay) (setComp Par arr)
        ]
    ]

main :: IO ()
main = do
  withGlobalScheduler_ globalScheduler $ \scheduler ->
    defaultMain
      [ anyBench scheduler "None" arrayNone
      , anyBench scheduler "Late" arrayLate
      , anyBench scheduler "Early" arrayEarly
      -- , waldoBenchmark scheduler 0 1000
      -- , waldoBenchmark scheduler 531186389 1000
      ]

waldoBenchmark :: Scheduler IO () -> Int32 -> Int32 -> Benchmark
waldoBenchmark scheduler waldo k =
  env (pure (a, v, l)) $ \ ~(vec, vecVP, ls) ->
    bgroup
      ("Waldo/" ++ show waldo)
      [ bench "list" $ whnf (Prelude.any (hasWaldo waldo)) ls
      , bench "parList" $
        whnf
          (\xs -> Prelude.or (Prelude.map (hasWaldo waldo) xs `using` parList rseq)) ls
      , bench "parList (performGC)" $
        whnfIO
          (Prelude.or (Prelude.map (hasWaldo waldo) ls `using` parList rseq) <$ performGC)
      , bench "vector" $ whnf (VP.any (hasWaldo waldo)) vecVP
      , bench "anySu" $ whnf (anySu (hasWaldo waldo) . delay) vec
      , bench "anySur" $ whnf (anySur (hasWaldo waldo) . delay) vec
      , bench "anyPi" $ whnfIO (anyPi (hasWaldo waldo) (setComp Par vec))
      , bench "anyPig" $ whnfIO (anyPig scheduler (hasWaldo waldo) (setComp Par vec))
      , bench "anyPur" $ whnfIO (anyPur (hasWaldo waldo) (delay (setComp Par vec)))
      , bench "anyS" $ whnf (anyS (hasWaldo waldo) . delay) vec
      , bench "anyP" $ whnfIO (anyP (hasWaldo waldo) (setComp Par vec))
      , bench "massiv (Seq)" $
        whnf (A.any (hasWaldo waldo) . delay) (setComp Seq vec)
      , bench "massiv (Par)" $
        whnf (A.any (hasWaldo waldo) . delay) (setComp Par vec)
      ]
  where
    l = [1 .. k] :: [Int32]
    v = VP.fromList l :: VP.Vector Int32
    a = A.fromList Seq l :: Array P Ix1 Int32


lcgs :: Int32 -> [Int32]
lcgs = iterate lcg


hasWaldo' :: Int32 -> Int32 -> Bool
hasWaldo' waldo x = waldo `Prelude.elem` Prelude.take 400000 (lcgs x)

lcg :: Int32 -> Int32
lcg x = 1664525 * x + 1013904223

hasWaldo :: Int32 -> Int32 -> Bool
hasWaldo waldo = go 0
  where
    k = 400000 :: Int
    go i x
      | i < k =
        let !x' = lcg x
         in waldo == x' || go (i + 1) x'
      | otherwise = False




-- anyP :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP f arr = do
--     let sz = size arr
--         totalLength = totalElem sz
--     results <-
--       withScheduler (getComp arr) $ \scheduler ->
--         splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--           loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--             scheduleWork scheduler $ do
--               loopM_ start (< start + chunkLength) (+ 1) $ \ !i ->
--                 when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
--               pure False
--           when (slackStart < totalLength) $
--             scheduleWork scheduler $ do
--               loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
--                 when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
--               pure False
--     pure $ F.foldl' (||) False results
-- {-# INLINE anyP #-}


-- anyP' :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP' f =
--   ifoldlScheduler
--     not
--     (\scheduler _ _ e ->
--        if f e then pure True --terminateWith scheduler True
--        else pure False)
--     False
--     (\acc a -> pure (acc || a))
--     False
-- {-# INLINE anyP' #-}


-- anyP' :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP' f =
--   ifoldlScheduler
--     not
--     (\_ acc _ e -> pure (acc || f e))
--     False
--     (\acc a -> pure (acc || a))
--     False
-- {-# INLINE anyP' #-}

-- No checkpoints
ifoldlScheduler ::
     (MonadUnliftIO m, Source r ix e)
  => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
  -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
  -> a -- ^ Accumulator
  -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
  -> b -- ^ Accumulator for chunks folding
  -> Array r ix e
  -> m b
ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
  let !sz = size arr
      !totalLength = totalElem sz
      !comp = getComp arr
  results <-
    withScheduler comp $ \scheduler -> do
      batch <- getCurrentBatch scheduler
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        let iterShort len start =
              scheduleWork scheduler $
              loopMaybeM start (< start + len) (+ 1) iAcc $ \i acc ->
                if fcont acc
                  then do
                    done <- hasBatchFinished batch
                    if done
                      then do
                        --liftIO yield
                        pure Nothing
                      else do
                        let e = unsafeLinearIndex arr i
                        res <- f scheduler acc (fromLinearIndex sz i) e
                        pure $ Just res
                  else do
                  Nothing <$ cancelBatch batch acc
        loopM_ 0 (< slackStart) (+ chunkLength) (iterShort chunkLength)
        when (slackStart < totalLength) $
          iterShort (totalLength - slackStart) slackStart
  F.foldlM g tAcc results
{-# INLINE ifoldlScheduler #-}


-- ifoldlScheduler ::
--      (MonadUnliftIO m, Source r ix e)
--   => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
--   -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
--   -> a -- ^ Accumulator
--   -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
--   -> b -- ^ Accumulator for chunks folding
--   -> Array r ix e
--   -> m b
-- ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !comp = getComp arr
--   results <-
--     withScheduler comp $ \scheduler ->
--       if numWorkers scheduler == 1
--         then scheduleWork scheduler $ ifoldlShort fcont (f scheduler) iAcc arr
--         else splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--                let --checkPoint = max 1 (chunkLength `div` (numWorkers scheduler * 100))
--                    iterShort len start =
--                      scheduleWork scheduler $
--                      cpLoopMaybeM 0 (< len) (+ 1) iAcc 1 $ \i acc cp ->
--                        if fcont acc
--                          then do
--                            let i' = i + start
--                            let e = unsafeLinearIndex arr i'
--                            if i == cp
--                              then do
--                                done <- isTerminated scheduler
--                                if done
--                                  then Nothing <$ liftIO yield
--                                  else do
--                                    res <-
--                                      f scheduler acc (fromLinearIndex sz i') e
--                                    pure $ Just (res, cp + cp)
--                              else do
--                                res <- f scheduler acc (fromLinearIndex sz i') e
--                                pure $ Just (res, cp)
--                          else pure Nothing
--                    {-# INLINE iterShort #-}
--                loopM_ 0 (< slackStart) (+ chunkLength) (iterShort chunkLength)
--                when (slackStart < totalLength) $
--                  iterShort (totalLength - slackStart) slackStart
--   F.foldlM g tAcc results
-- {-# INLINE ifoldlScheduler #-}


cpLoopMaybeM ::
     Monad m
  => b
  -> (b -> Bool)
  -> (b -> b)
  -> a
  -> b
  -> (b -> a -> b -> m (Maybe (a, b)))
  -> m a
cpLoopMaybeM !startAt condition increment !initAcc initCheckPoint f =
  go startAt initAcc initCheckPoint
  where
    go !step !acc cp
      | condition step =
        f step acc cp >>= \case
          Just (acc', cp') -> go (increment step) acc' cp'
          Nothing          -> pure acc
      | otherwise = pure acc
{-# INLINE cpLoopMaybeM #-}




-- V1
-- ifoldlScheduler ::
--      (MonadUnliftIO m, Source r ix e)
--   => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
--   -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
--   -> a -- ^ Accumulator
--   -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
--   -> b -- ^ Accumulator for chunks folding
--   -> Array r ix e
--   -> m b
-- ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !comp = getComp arr
--   results <-
--     withScheduler comp $ \scheduler ->
--       splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--         let cp = chunkLength `div` numWorkers scheduler
--             cpAction = do
--               done <- isTerminated scheduler
--               when done $ liftIO yield
--         loopM_ 0 (< slackStart) (+ chunkLength) $ \ start ->
--           let stop = start + chunkLength
--            in scheduleWork scheduler $
--               loopMaybeM start (< stop) (+ 1) iAcc $ \i acc ->
--                 if fcont acc
--                   then do
--                     let e = unsafeLinearIndex arr i
--                     res <- f scheduler acc (fromLinearIndex sz i) e
--                     pure $ Just res
--                   else pure Nothing
--         when (slackStart < totalLength) $
--           scheduleWork scheduler $
--           loopMaybeM slackStart (< totalLength) (+ 1) iAcc $ \i acc ->
--             if fcont acc
--               then do
--                 let e = unsafeLinearIndex arr i
--                 res <- f scheduler acc (fromLinearIndex sz i) e
--                 pure $ Just res
--               else pure Nothing
--           -- iterShortM sz slackStart totalLength 1 cp cpAction (<) iAcc $ \ !i ix !acc -> do
--           --   f scheduler acc ix (unsafeLinearIndex arr i)
--   F.foldlM g tAcc results
-- {-# INLINE ifoldlScheduler #-}


-- -- V2
-- ifoldlScheduler ::
--      (MonadUnliftIO m, Source r ix e)
--   => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
--   -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
--   -> a -- ^ Accumulator
--   -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
--   -> b -- ^ Accumulator for chunks folding
--   -> Array r ix e
--   -> m b
-- ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !comp = getComp arr
--   results <-
--     withScheduler comp $ \scheduler ->
--       splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--         let cp = max 1 (chunkLength `div` numWorkers scheduler)
--             iterShort len start =
--               scheduleWork scheduler $
--               loopMaybeM 0 (< len) (+ 1) iAcc $ \i acc ->
--                 if fcont acc
--                   then do
--                     let i' = i + start
--                     let e = unsafeLinearIndex arr i'
--                     if i == cp
--                       then do
--                         done <- isTerminated scheduler
--                         if done
--                           then do
--                             liftIO yield
--                             pure Nothing
--                           else do
--                             res <- f scheduler acc (fromLinearIndex sz i') e
--                             pure $ Just res
--                       else do
--                         res <- f scheduler acc (fromLinearIndex sz i') e
--                         pure $ Just res
--                   else pure Nothing
--         loopM_ 0 (< slackStart) (+ chunkLength) (iterShort chunkLength)
--         when (slackStart < totalLength) $
--           iterShort (totalLength - slackStart) slackStart
--   F.foldlM g tAcc results
-- {-# INLINE ifoldlScheduler #-}



-- V3
-- ifoldlScheduler ::
--      (MonadUnliftIO m, Source r ix e)
--   => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
--   -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
--   -> a -- ^ Accumulator
--   -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
--   -> b -- ^ Accumulator for chunks folding
--   -> Array r ix e
--   -> m b
-- ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !comp = getComp arr
--   results <-
--     withScheduler comp $ \scheduler ->
--       splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--         let checkPoint = chunkLength `div` (numWorkers scheduler * 1000)
--             iterShort len start =
--               scheduleWork scheduler $
--               cpLoopMaybeM 0 (< len) (+ 1) iAcc checkPoint $ \i acc cp ->
--                 if fcont acc
--                   then do
--                     let i' = i + start
--                     let e = unsafeLinearIndex arr i'
--                     if i == cp
--                       then do
--                         done <- isTerminated scheduler
--                         --liftIO (putStrLn $ "CP: " ++ show cp ++ ", Done: " ++ show done)
--                         if done
--                           then Nothing <$ liftIO yield
--                           else do
--                             res <- f scheduler acc (fromLinearIndex sz i') e
--                             pure $ Just (res, cp + checkPoint)
--                       else do
--                         res <- f scheduler acc (fromLinearIndex sz i') e
--                         pure $ Just (res, cp)
--                   else pure Nothing
--         loopM_ 0 (< slackStart) (+ chunkLength) (iterShort chunkLength)
--         when (slackStart < totalLength) $
--           iterShort (totalLength - slackStart) slackStart
--   F.foldlM g tAcc results
-- {-# INLINE ifoldlScheduler #-}



-- V4
-- ifoldlScheduler ::
--      (MonadUnliftIO m, Source r ix e)
--   => (a -> Bool) -- ^ Should continue going, i.e. short ciscuit on `False`
--   -> (Scheduler m a -> a -> ix -> e -> m a) -- ^ Index aware folding IO action
--   -> a -- ^ Accumulator
--   -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
--   -> b -- ^ Accumulator for chunks folding
--   -> Array r ix e
--   -> m b
-- ifoldlScheduler fcont f !iAcc g !tAcc !arr = do
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !comp = getComp arr
--   results <-
--     withScheduler comp $ \scheduler ->
--       if numWorkers scheduler == 1
--         then scheduleWork scheduler $ ifoldlShort fcont (f scheduler) iAcc arr
--         else splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--                let checkPoint = chunkLength `div` (numWorkers scheduler * 1000)
--                    iterShort len start =
--                      scheduleWork scheduler $
--                      cpLoopMaybeM 0 (< len) (+ 1) iAcc checkPoint $ \i acc cp ->
--                        if fcont acc
--                          then do
--                            let i' = i + start
--                            let e = unsafeLinearIndex arr i'
--                            if i == cp
--                              then do
--                                done <- isTerminated scheduler
--                                if done
--                                  then Nothing <$ liftIO yield
--                                  else do
--                                    res <-
--                                      f scheduler acc (fromLinearIndex sz i') e
--                                    pure $ Just (res, cp + checkPoint)
--                              else do
--                                res <- f scheduler acc (fromLinearIndex sz i') e
--                                pure $ Just (res, cp)
--                          else pure Nothing
--                    {-# INLINE iterShort #-}
--                loopM_ 0 (< slackStart) (+ chunkLength) (iterShort chunkLength)
--                when (slackStart < totalLength) $
--                  iterShort (totalLength - slackStart) slackStart
--   F.foldlM g tAcc results
-- {-# INLINE ifoldlScheduler #-}













foldlList :: (a -> b -> a) -> a -> [b] -> a
foldlList f = go
  where
    go !acc []     = acc
    go !acc (x:xs) = go (f acc x) xs

foldrList :: (b -> a -> a) -> a -> [b] -> a
foldrList f = go
  where
    go acc []     = acc
    go acc (x:xs) = f x (go acc xs)
{-# INLINE foldrList #-}

foldlArray :: Source r ix b => (a -> b -> a) -> a -> Array r ix b -> a
foldlArray f initAcc arr = go 0 initAcc
  where
    k = elemsCount arr
    go i !acc
      | i < k = go (i + 1) (f acc (unsafeLinearIndex arr i))
      | otherwise = acc

foldrArray :: Source r ix b => (b -> a -> a) -> a -> Array r ix b -> a
foldrArray f initAcc arr = go 0 initAcc
  where
    !k = elemsCount arr
    go !i !acc
      | i < k = f (unsafeLinearIndex arr i) (go (i + 1) acc)
      | otherwise = acc
{-# INLINE foldrArray #-}
