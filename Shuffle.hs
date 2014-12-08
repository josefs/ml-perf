module Shuffle where

{- Shuffling elements around randomly
   Based on an algorithm by Oleg
-}

import System.Random

data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving Show

-- Convert a non-empty sequence (e1...en) to a complete binary tree
build_tree = grow_level . (map Leaf)
    where
    grow_level [node] = node
    grow_level l = grow_level $ inner l
	     
    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = (join e1 e2) : inner rest
	     
    join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
    join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
    join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r

{-
-- example:
Main> build_tree ['a','b','c','d','e']
Node 5 (Node 4 (Node 2 (Leaf 'a') (Leaf 'b'))
               (Node 2 (Leaf 'c') (Leaf 'd')))
       (Leaf 'e')

-}

-- given a non-empty sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements rseq = shuffle1' (build_tree elements) rseq
    where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) = extract_tree ri tree 
				    (\tree -> shuffle1' tree r_others)
	     -- extract_tree n tree
	     -- extracts the n-th element from the tree and returns
	     -- that element, paired with a tree with the element
	     -- deleted (only instead of pairing, we use CPS)
	     -- The function maintains the invariant of the completeness
	     -- of the tree: all internal nodes are always full.
	     -- The collection of patterns below is deliberately not complete.
	     -- All the missing cases may not occur (and if they do,
	     -- that's an error.
    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf r)) k = r:k l
    extract_tree n (Node c l@Leaf{} r) k =
	extract_tree (n-1) r (\new_r -> k $ Node (c-1) l new_r)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
				       
    extract_tree n (Node c l@(Node cl _ _) r) k
	| n < cl = extract_tree n l (\new_l -> k $ Node (c-1) new_l r)
	| otherwise = extract_tree (n-cl) r (\new_r -> k $ Node (c-1) l new_r)

-- Given a source of randomness g and an integer n>0, produce
-- a list of [r1,...rn] of numbers such that ri is an independent sample
-- from a uniform random distribution [0..n-i+1]

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
  loop acc 0 g = (reverse acc,g)
  loop acc n g = let (r,g') = randomR (0,n) g 
		 in loop (r:acc) (pred n) g'

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle r ls = shuffle1 ls p
  where (p,_) = make_rs (length ls - 1) r
