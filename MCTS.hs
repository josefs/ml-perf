{-# LANGUAGE BangPatterns #-}
module MCTS where

import System.Random
import Data.List
import Data.Array
import Data.Function
import Control.Applicative
import Text.Show.Pretty

import Control.Exception.Assert
import Debug.Trace

import Monad
import Parser
import Dim

type Params = [(Int,Int)]
type PartialChoice = ([Int],[Int])
type Choice = [Int]
type Cache = [([Int],Time)]

data Settings = Settings {
    noSamples :: Int,
    rankTime  :: [Time] -> Time
  }

mcts :: RandomGen g =>
          Settings ->
          Params -> [Int] ->
          (Dim n -> MMT n g Time) ->
          MMT n g (Dim n)
mcts settings params branch eval = search settings [] params branch eval

search :: RandomGen g => 
          Settings -> [Int] -> Params -> [Int] ->
          (Dim n  -> MMT n g Time) ->
          MMT n g [Int]
search _ fixedParams [] _ _ = return fixedParams
search settings fixedParams pendingParams (branch:b) eval = do
  cs <- playouts branch pendingParams
  assert (all (\c -> length c == length pendingParams) cs) (return ())
  evalCands <- lookupCand (map (\c -> (fixedParams,c)) cs) eval
  trace (ppShow evalCands) (return ())
  let sortedCands = groupBy ((==) `on` firstVariable)
                    (sortBy comparingTime evalCands)
      (_,ls) = minimumBy (compare `on` fst)
                 (rank (rankTime settings) sortedCands)
      choice = head (snd (fst (head ls)))
  search settings (fixedParams++[choice]) (tail pendingParams) b eval
 where
    comparingTime = compare `on` (fst.fst)
    firstVariable = head.snd.fst

rank f cs = map (\c -> (f [t | (_,t) <- c ],c)) cs

lookupCand :: (RandomGen g, MkDim n) => 
              [([Int],[Int])] -> (Dim n -> MMT n g Time) ->
              MMT n g [(([Int],[Int]),Time)]
lookupCand cs eval = mapM (\c -> do trace (ppShow (uncurry (++) c)) (return ())
                                    t <- eval (listToDim (uncurry (++) c))
                                    return (c,t)) cs

playouts :: RandomGen g => Int -> [(Int,Int)] -> MMT n g [[Int]]
playouts noSamples pendingParams =
  mapM mkRandomSample (replicate noSamples pendingParams)

mkRandomSample :: RandomGen g => [(Int,Int)] -> MMT n g [Int]
mkRandomSample ls = mapM rand ls

-- Data processing

mkEval :: RandomGen g => Array (Dim n) Time -> Dim n -> MMT n g Time
mkEval = undefined
{-
mkEval :: [[Time]] -> [Int] -> MMT n StdGen Time
mkEval d params = 
  case params of
    [dmd,inner,outer] -> do
      mt <- checkCache params
      case mt of
        Nothing -> do
          let t = ((d!!(outer*4+inner))!!dmd)
          addTime t
          addToCache (params,t)
          return t
        Just t -> return t
    _ -> error ("Erronious parameters: \n" ++ ppShow params)
-}
isComment ('#':_) = True
isComment _ = False


main2 = parse "graphs.txt" $ \arr -> do
          let f = mkEval arr
          g <- newStdGen
          let !(p,t) = runMMT (mcts defaultSettings [(0,2),(0,9),(0,0)] [15,8,1] f) g
          putStrLn ("Best parameter: \n" ++ ppShow p)
          putStrLn ("Estimation time: " ++ show t)
          let total = sum (elems arr)
          putStrLn ("Total time: " ++ show total)
          let best = minimum (elems arr)
          putStrLn ("Best time: " ++ show best)
          let est = runEval f p
          putStrLn ("Suggested time: " ++ show est)
          putStrLn (show ((est-best)/best*100) ++ "%")
{-
main = do d <- readData
          let f = mkEval d
          g <- newStdGen
          let !(p,t) = runMMT (mcts defaultSettings [(0,2),(0,9),(0,0)] [15,8,1] f) g
          putStrLn ("Best parameter: \n" ++ ppShow p)
          putStrLn ("Estimation time: " ++ show t)
          let total = sum (map sum d)
          putStrLn ("Total time: " ++ show total)
          let best = minimum (map minimum d)
          putStrLn ("Best time: " ++ show best)
          let est = runEval f p
          putStrLn ("Suggested time: " ++ show est)
          putStrLn (show ((est-best)/best*100) ++ "%")
-}

defaultSettings = Settings 4 leastSquares

avg ls = sum ls / fromIntegral (length ls)

leastSquares :: Fractional a => [a] -> a
leastSquares ls = sum (map (^(2::Int)) ls) / fromIntegral (length ls)
