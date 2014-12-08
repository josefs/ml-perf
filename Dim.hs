{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Dim where

import Data.Ix
import System.Random
import Data.List (foldl')

data Nat = Succ Nat | Zero

data Dim :: Nat -> * where
  Empty :: Dim 'Zero
  Dim :: Int -> Dim n -> Dim ('Succ n)

instance Show (Dim n) where
  show Empty = "[]"
  show (Dim n d) = show n ++ " : " ++ show d

class MkDim n where
  listToDim :: [Int] -> Dim n

instance MkDim 'Zero where
  listToDim _ = Empty

instance MkDim n => MkDim ('Succ n) where
  listToDim (n:ns) = Dim n (listToDim ns)

data Sing :: Nat -> * where
  SZ :: Sing Zero
  SS :: Sing n -> Sing (Succ n)

neighbours :: Dim d -> (Dim d, Dim d) -> [Dim d]
neighbours Empty (Empty,Empty) = []
neighbours (Dim i d) r@(Dim l rl, Dim u ru) =
  filter (inRange r) [Dim (i-1) d, Dim (i+1) d] ++
     map (Dim i) (neighbours d (rl,ru))

instance Eq (Dim n) where
  Empty == Empty = True
  Dim i di == Dim j dj = i == j && di == dj

instance Ord (Dim n) where
  Empty <= Empty = True
  Dim i di <= Dim j dj = i < j || i == j && di <= dj

instance Ix (Dim n) where
  inRange (Empty,Empty) Empty = True
  inRange (Dim l dl, Dim u du) (Dim i di) =
    l <= i && i <= u && inRange (dl,du) di
  range (Empty,Empty) = [Empty]
  range (Dim l dl, Dim u du) =
    do i <- [l..u]
       r <- range (dl,du)
       return (Dim i r)
  index (Empty,Empty) Empty = 0
  index (Dim l dl,Dim u du) (Dim i di) = n*(u-l+1) + (i-l)
    where n = index (dl,du) di

getRange :: [Dim n] -> (Dim n,Dim n)
getRange (d:ds) = loop (d,d) ds

loop :: (Dim n, Dim n) -> [Dim n] -> (Dim n,Dim n)
loop = foldl' update 

update :: (Dim n, Dim n) -> Dim n -> (Dim n, Dim n)
update (Empty,Empty) Empty = (Empty,Empty)
update (Dim l dl, Dim u du) (Dim i di)
  = (Dim (min l i) lr,Dim (max i u) ur)
  where (lr,ur) = update (dl,du) di

randomPoint :: RandomGen g => (Dim n, Dim n) -> g -> (Dim n,g)
randomPoint (Empty,Empty) g = (Empty,g)
randomPoint (Dim l dl, Dim u du) g = (Dim i p,g'')
  where (i,g') = randomR (l,u) g
        (p,g'') = randomPoint (dl,du) g'

instance {- MkDim n => -} Random (Dim n) where
  randomR = randomPoint
{- Live dangerously for now
  random g = (listToDim (randoms g1),g2)
    where (g1,g2) = split g
-}
