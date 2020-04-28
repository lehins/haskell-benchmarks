{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Frames
import qualified Data.Foldable as F
import           Control.Lens (to, only,(^?),ix, toListOf, (^.))
import           Data.Vinyl (Rec(..))
import qualified Language.R as R
import           Language.R.QQ
import qualified Data.Text as T
import           Data.List


tableTypes "Benchmarks" "old-random-word16.csv"

loadBenchmarks :: IO (Frame Benchmarks)
loadBenchmarks = inCoreAoS (readTable "old-random-word16.csv")

loadNewBenchmarks :: IO (Frame Benchmarks)
loadNewBenchmarks = inCoreAoS (readTable "new-random-word16.csv")

declareColumn "oldMean" ''Double
declareColumn "newMean" ''Double
declareColumn "testName" ''Text

interleave :: [a] -> [a] -> [a]
interleave = curry $ unfoldr g
  where
    g ([], [])   = Nothing
    g ([], (y:ys)) = Just (y, (ys, []))
    g (x:xs, ys) = Just (x, (ys, xs))

main :: IO ()
main = do
  bs <- loadBenchmarks
  cs <- loadNewBenchmarks

  let ns :: Frame (Record '[TestName])
      ns = fmap ((&: RNil) . (^. name)) bs
      ms :: Frame (Record '[OldMean])
      ms = fmap ((&: RNil) . (^. mean)) bs
      ps :: Frame (Record '[NewMean])
      ps = fmap ((&: RNil) . (^. mean)) cs
      qs :: Frame (Record '[TestName, OldMean, NewMean])
      qs = zipFrames ns (zipFrames ms ps)
  mapM_ print ((F.toList qs))

  let as, bs :: [Double]
      as = F.toList $ fmap (^. oldMean) qs
      bs = F.toList $ fmap (^. newMean) qs
      xs = map (* 1000) $ interleave as bs
      cs :: [String]
      cs = map T.unpack $ F.toList $ fmap (^. testName) qs
      ds :: [String]
      ds = concatMap (replicate 2) cs
      l = length ds
      es :: [String]
      es = take l (cycle ["Random 1.1", "Random 1.2"])

  R.runRegion $ do
    [r| library(ggplot2) |]
    df <- [r| data <- data.frame(ds_hs, es_hs, xs_hs) |]
    p1 <- [r| ggplot(df_hs, aes(fill=es_hs, y=xs_hs, x=ds_hs, label=xs_hs)) |]
    p2 <- [r| p1_hs + geom_bar(position="dodge", stat="identity") |]
    p3 <- [r| p2_hs + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) |]
    p4 <- [r| p3_hs + ylab("Microseconds") + xlab("Type") +labs(fill = "Version") + scale_fill_manual(values=c("lightskyblue", "pink")) + geom_text(size = 2, position = position_dodge(width = 1), aes(y=xs_hs + 5.0, label=signif(xs_hs, digits=4), hjust=0), angle=90) |]
    p5 <- [r| p4_hs + ggtitle("RNG Performance Improvement") + theme(plot.title = element_text(hjust = 0.5)) |]
    [r| ggsave(filename="diagrams/Compare1.1Vs1.2.svg") |]
    return ()

