module Statistics where

import System.IO
import Statistics.Sample.Histogram
import Statistics.Sample.KernelDensity
import Data.Vector.Unboxed hiding ((++))

import Parser
import Monad

mkHist n file out = do
  ts <- parseTimes file :: IO [Double]
  let v = fromList ts
      (lb,hist) = histogram n v
  h <- openFile out WriteMode
  for2 lb hist $ \a b ->
    hPutStrLn h (show a ++ "\t" ++ show (b :: Int))
  hClose h

for2 v1 v2 f = zipWithM_ f v1 v2

writeVector v out = do
  h <- openFile out WriteMode
  forM_ v $ \a ->
    hPutStrLn h (show a)
  hClose h

mkKDE n file out = do
  ts <- parseTimes file :: IO [Double]
  let v = fromList ts
      (lb,hist) = kde n v
  h <- openFile out WriteMode
  for2 lb hist $ \a b ->
    hPutStrLn h (show a ++ "\t" ++ show b)
  hClose h
