{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Criterion.Main
import Control.Concurrent
import Control.Scheduler
import Control.Monad
--import Data.Functor.Identity
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import qualified Data.Vector.Primitive as VP
import Prelude
import Data.Int
import Control.Parallel.Strategies
import System.Mem

loopMaybeM :: Monad m => b -> (b -> Bool) -> (b -> b) -> a -> (b -> a -> m (Maybe a)) -> m a
loopMaybeM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc
      | condition step = f step acc >>= \case
          Just acc' -> go (increment step) acc'
          Nothing -> pure acc
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


loopShortM :: Monad m => Int -> (Int -> a -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopShortM !startAt condition increment !initAcc f = go startAt initAcc
  where
    go !step !acc
      | condition step acc = f step acc >>= go (increment step)
      | otherwise = return acc
{-# INLINE loopShortM #-}



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


foldlList :: (a -> b -> a) -> a -> [b] -> a
foldlList f = go
  where
    go !acc [] = acc
    go !acc (x:xs) = go (f acc x) xs

foldrList :: (b -> a -> a) -> a -> [b] -> a
foldrList f = go
  where
    go acc [] = acc
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

anyS :: Source r ix a => (a -> Bool) -> Array r ix a -> Bool
--anyS f = runIdentity . foldlShort not (\acc e -> acc || f e) False -- <== almost as fast
anyS f arr = go 0
  where
    !k = elemsCount arr
    go i
      | i < k = f (unsafeLinearIndex arr i) || go (i + 1)
      | otherwise = False
{-# INLINE anyS #-}

-- anyP :: (Source r Ix1 e, Source r ix e) => (e -> Bool) -> Array r ix e -> IO Bool
-- anyP f = splitReduce (\s arr -> pure $ anyS f arr) (\r a -> pure (a || r)) False

ifoldlShort ::
     (Monad m, Source r ix e)
  => (a -> Bool)
  -> (a -> ix -> e -> m a)
  -> a
  -> Array r ix e
  -> m a
ifoldlShort g f initAcc arr =
  loopMaybeM
    0
    (< k)
    (+ 1)
    initAcc
    (\i acc ->
       if g acc
         then Just <$> f acc (fromLinearIndex sz i) (unsafeLinearIndex arr i)
         else pure Nothing)
  where
    sz = size arr
    k = totalElem sz
{-# INLINE ifoldlShort #-}

foldlShort ::
     Monad m => Source r ix e => (a -> Bool) -> (a -> e -> a) -> a -> Array r ix e -> m a
foldlShort g f initAcc arr =
  loopMaybeM
    0
    (< k)
    (+ 1)
    initAcc
    (\i acc ->
       pure $
       if g acc
         then Just $ f acc (unsafeLinearIndex arr i)
         else Nothing)
  where
    k = elemsCount arr
{-# INLINE foldlShort #-}

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
              loopShortM start (\i a -> not a && i < start + chunkLength) (+ 1) False $ \ !i acc ->
                pure (acc || f (unsafeLinearIndex arr i))
            if res
              then void (terminateWith scheduler True) >> pure True
              else pure res
            -- loopM_ start (< start + chunkLength) (+ 1) $ \ !i ->
            --   when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
            -- pure False
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
              when (f (unsafeLinearIndex arr i)) $ void $ terminateWith scheduler True
            pure False
  pure $ F.foldl' (||) False results
{-# INLINE anyP #-}



-- arrayLate :: Array P Ix1 Int
-- arrayLate =
--   computeAs P $
--   concat' 1 [A.enumFromStepN Seq 1 2 2999999, A.singleton 2, A.enumFromStepN Seq 1 2 1000000]
arrayLate :: Array P Ix1 Int
arrayLate =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 7000000, A.singleton 2, A.enumFromStepN Seq 1 2 999999]


arrayLateShort :: Array P Ix1 Int
arrayLateShort =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 7000, A.singleton 2, A.enumFromStepN Seq 1 2 999]

arrayEarly :: Array P Ix1 Int
arrayEarly =
  computeAs P $
  concat' 1 [A.singleton 2, A.enumFromStepN Seq 1 2 7000000, A.enumFromStepN Seq 1 2 999999]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "any"
        [ env (pure (arrayLate, A.toVector arrayLate, A.toList arrayLate)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "Late"
              [ bench "list" $ whnfIO (pure $ Prelude.any even ls)
              , bench "vector" $ whnfIO (pure $ VP.any even vecVP)
              , bench "anyS" $ whnfIO (pure $ anyS even vec)
              , bench "anyP" $ whnfIO (anyP even (setComp Par vec))
              , bench "anyS'" $ whnfIO (anyP' even (setComp Seq vec))
              , bench "anyP'" $ whnfIO (anyP' even (setComp Par vec))
              , bench "massiv (Seq)" $ whnfIO (pure $ A.any even (setComp Seq vec))
              , bench "massiv (Par)" $ whnfIO (pure $ A.any even (setComp Par vec))
              ]
        , env
            (pure
               ( arrayLateShort
               , A.toVector arrayLateShort
               , A.toList arrayLateShort)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "LateShort"
              [ bench "list" $ whnfIO (pure $ Prelude.any even ls)
              , bench "vector" $ whnfIO (pure $ VP.any even vecVP)
              , bench "anyS" $ whnfIO (pure $ anyS even vec)
              , bench "anyP" $ whnfIO (anyP even (setComp Par vec))
              , bench "anyS'" $ whnfIO (anyP' even (setComp Seq vec))
              , bench "anyP'" $ whnfIO (anyP' even (setComp Par vec))
              , bench "massiv (Seq)" $ whnfIO (pure $ A.any even (setComp Seq vec))
              , bench "massiv (Par)" $ whnfIO (pure $ A.any even (setComp Par vec))
              ]
        , env (pure (arrayEarly, A.toVector arrayEarly, A.toList arrayEarly)) $ \ ~(vec, vecVP, ls) ->
            bgroup
              "Early"
              [ bench "list" $ whnfIO (pure $ Prelude.any even ls)
              , bench "vector" $ whnfIO (pure $ VP.any even vecVP)
              , bench "anyS" $ whnfIO (pure $ anyS even vec)
              , bench "anyP" $ whnfIO (anyP even (setComp Par vec))
              , bench "anyS'" $ whnfIO (anyP' even (setComp Seq vec))
              , bench "anyP'" $ whnfIO (anyP' even (setComp Par vec))
              , bench "massiv (Seq)" $ whnfIO (pure $ A.any even (setComp Seq vec))
              , bench "massiv (Par)" $ whnfIO (pure $ A.any even (setComp Par vec))
              ]
        , waldoBenchmark 0 100
        , waldoBenchmark 531186389 100
        ]
    ]

waldoBenchmark :: Int32 -> Int32 -> Benchmark
waldoBenchmark waldo k =
  env (pure (a, v, l)) $ \ ~(vec, vecVP, ls) ->
    bgroup
      ("Waldo/" ++ show waldo)
      [ bench "list" $ whnfIO (pure $ Prelude.any (hasWaldo waldo) ls)
      , bench "parList" $
        whnfIO
          (pure $
           Prelude.or (Prelude.map (hasWaldo waldo) ls `using` parList rseq))
      , bench "parList (performGC)" $
        whnfIO
          (Prelude.or (Prelude.map (hasWaldo waldo) ls `using` parList rseq) <$ performGC)
      , bench "vector" $ whnfIO (pure $ VP.any (hasWaldo waldo) vecVP)
      , bench "anyS" $ whnfIO (pure $ anyS (hasWaldo waldo) vec)
      , bench "anyP" $ whnfIO (anyP (hasWaldo waldo) (setComp Par vec))
      , bench "anyS'" $ whnfIO (anyP' (hasWaldo waldo) (setComp Seq vec))
      , bench "anyP'" $ whnfIO (anyP' (hasWaldo waldo) (setComp Par vec))
      , bench "massiv (Seq)" $
        whnfIO (pure $ A.any (hasWaldo waldo) (setComp Seq vec))
      , bench "massiv (Par)" $
        whnfIO (pure $ A.any (hasWaldo waldo) (setComp Par vec))
      ]
  where
    l = [1 .. k] :: [Int32]
    v = VP.fromList l :: VP.Vector Int32
    a = A.fromList Seq l :: Array P Ix1 Int32


lcgs :: Int32 -> [Int32]
lcgs = iterate lcg
  where lcg x = 1664525 * x + 1013904223

hasWaldo :: Int32 -> Int32 -> Bool
hasWaldo waldo x = waldo `elem` Prelude.take 400000 (lcgs x)




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


anyP' :: Source r ix e => (e -> Bool) -> Array r ix e -> IO Bool
anyP' f =
  ifoldlScheduler
    not
    (\scheduler _ _ e ->
       if f e then terminateWith scheduler True
       else pure False)
    False
    (\acc a -> pure (acc || a))
    False
{-# INLINE anyP' #-}


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
    withScheduler comp $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        let iterShort len start =
              scheduleWork scheduler $
              loopMaybeM start (< start + len) (+ 1) iAcc $ \i acc ->
                if fcont acc
                  then do
                    done <- isTerminated scheduler
                    if done
                      then do
                        liftIO yield
                        pure Nothing
                      else do
                        let e = unsafeLinearIndex arr i
                        res <- f scheduler acc (fromLinearIndex sz i) e
                        pure $ Just res
                  else pure Nothing
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
          Nothing -> pure acc
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
