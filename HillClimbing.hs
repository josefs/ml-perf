{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module HillClimbing where

import Data.Array
import Data.List
import Data.Function
import Control.Applicative
import System.Random

import Dim
import Parser
import Monad

hillClimbing :: RandomGen g =>
                Array (Dim n) Time -> Dim n -> MMT n g (Dim n, Time)
hillClimbing arr start = do
  let r = bounds arr
      t = arr!start
  climb arr r (start,t)

climb :: RandomGen g =>
         Array (Dim n) Time -> (Dim n, Dim n) -> (Dim n,Time) ->
         MMT n g (Dim n,Time)
climb arr range (point,t) = do
   ss <- mapM (climb arr range) ns
   return (minimumBy (compare `on` snd) ((point,t):ss))
  where n = neighbours point range
        ns = filter ((< t) . snd) $ map (\n -> (n,arr!n)) n

ordered (a:b:as) = a <= b && ordered (b:as)
ordered _ = True
{-
test = parse "fenix.grafo" (\a ->
           let (l,u) = bounds a in
           return ((a!l) :: Time))

test2 = parse "fenix.grafo" (\a -> do
           let (l,u) = bounds a
           g <- newStdGen
           ps <- sequence (replicate 10 (randomRIO (l,u)))
           let p = flip runMMT g $
                   fmap (minimumBy (compare `on` snd)) $
                   mapM (hillClimbing a) ps
           print p)
-}
