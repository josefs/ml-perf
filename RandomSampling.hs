{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module RandomSampling where

import Data.Array
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad
import System.Random
import Data.Maybe

import Dim
import Parser
import Monad
import Shuffle

-- Possibly sample the same point twice

sample :: RandomGen g =>
          Array (Dim n) Time -> Int -> MMT n g (Dim n, Time)
sample arr n = do
  points <- replicateM n (randG (l,u))
  samples <- mapM (lookUp arr) points
  return (minimumBy (compare `on` snd) samples)
  where (l,u) = bounds arr

lookUp :: RandomGen g =>
          Array (Dim n) Time -> Dim n -> MMT n g (Dim n, Time)
lookUp arr d = do mt <- checkCache d
                  case mt of
                    Just t -> return (d,t)
                    Nothing -> do addToCache (d,t)
                                  addTime t
                                  return (d,t)
  where t = arr!d

-- Make sure we don't sample the same point twice

randomSample :: RandomGen g =>
                Array (Dim n) Time -> MMT n g (Dim n, Time)
randomSample arr = do
   point <- randG range
   mt <- checkCache point
   case mt of
     Just _ -> randomSample arr
     Nothing -> do
       let t = arr ! point
       addToCache (point,t)
       addTime t
       return (point,t)
  where range = bounds arr

randomSampleM :: RandomGen g =>
                Array (Dim n) (Maybe Time) -> MMT n g (Dim n, Time)
randomSampleM arr = do
   point <- randG range
   mt <- checkCache point
   case (mt,arr!point) of
     (Just _,_) -> randomSampleM arr
     (Nothing,Nothing) -> randomSampleM arr
     (Nothing,Just t) -> do
       addToCache (point,t)
       addTime t
       return (point,t)
  where range = bounds arr

randomSampleN :: RandomGen g =>
                Array (Dim n) Time -> Int -> MMT n g (Dim n, Time)
randomSampleN arr n = do
  samples <- replicateM n (randomSample arr)
  return (minimumBy (compare `on` snd) samples)

randomSampleMN :: RandomGen g =>
                  Array (Dim n) (Maybe Time) -> Int -> MMT n g (Dim n, Time)
randomSampleMN arr n = do
  samples <- replicateM n (randomSampleM arr)
  return (minimumBy (compare `on` snd) samples)

test = parse "fenix.grafo" (\a _ -> do
           g <- newStdGen
           let ((_,t),total) = flip runMMT g $ sample a 20
           print t
           print total)

experiment arr n = do
  g <- newStdGen
  let ((_,t),total) = flip runMMT g $ sample arr n
  return (t,total)

exp2 arr n = do
  g <- newStdGen
  let ((_,t),total) = flip runMMT g $ randomSampleN arr n
  return (t,total)

sparseExperiment arr n = do
  g <- newStdGen
  let ((_,t),total) = flip runMMT g $ randomSampleMN arr n
  return (t,total)

testSparse file = parseSparse file (\a l -> do
  dia <- mapM (sparseExperiment a) [1..l]
  print dia)

totaltime file = parseSparse file (\a l -> do
  print $ sum $ catMaybes (elems a))

-- This version might sample the same point many times
diagramEx = parse "fenix.grafo" (\a l -> do
  dia <- mapM (experiment a) [1..l]
  print dia)

diagramEx2 = parse "fenix.grafo" (\a l -> do
  dia <- mapM (exp2 a) [1..l]
  print dia)

diagram file = parse file (\a l -> do
  dia <- mapM (exp2 a) [1..l]
  print dia)

-- Much better way of doing things:

expList ls n g = (minimum ts, sum ts)
  where ts = take n $ shuffle g ls

diagramList file = do
  ts <- parseTimes file :: IO [Time]
  zipWithM (\l r -> r >>= return . expList ts l) [1..length ts] (repeat newStdGen)

multipleDiagramList file n = do
  ts <- parseTimes file :: IO [Time]
  matrix <- replicateM n $
    zipWithM (\l r -> r >>= return . expList ts l) [1..length ts] (repeat newStdGen)
  let results = transpose matrix
  return results

boxAndWhisker file n out = do
  matrix <- multipleDiagramList file n
  writeFile out $ unlines [ intercalate " " $ map show bests | line <- matrix, let bests = map fst line ]
