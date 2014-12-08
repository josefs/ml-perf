{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module MCTS2 where

import Monad
import Dim
import System.Random

type VisitCount = Int
type Reward = Float
type State = [Int]
type Range = (Int,Int)

data TreeN n where
  LeafN :: TreeN 'Zero
  NodeN :: State -> Reward -> VisitCount -> [TreeN n] ->
           TreeN ('Succ n)

data Tree where
  Leaf :: Tree
  Node :: State -> Reward -> VisitCount -> [Tree] ->
          Tree

search :: RandomGen g => [Int] -> Tree -> [Range] ->
          ([Int] -> MMT n g Time) ->
          MMT n g [Int]
search fixedParams tree (range:rs) eval = undefined
  -- while
{-
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
-}
