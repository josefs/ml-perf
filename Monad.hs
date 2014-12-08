module Monad where

import System.Random
import Control.Applicative
import Dim

type Cache n = [(Dim n,Time)]
type Time = Float

data MMT n g a = MMT { unMMT :: g -> Cache n -> (a,Time, Cache n) }

instance Functor (MMT n g) where
  fmap f (MMT m) = MMT $ \g c -> case m g c of
                                 (a,t,c') -> (f a, t,c')

instance RandomGen g => Applicative (MMT n g) where
  pure = return
  MMT f <*> MMT a = MMT $ \g c1 ->
                    let (g1,g2) = split g in
                    case f g1 c1 of
                      (f',t1,c2) ->
                        case a g2 c2 of
                          (a',t2,c3) -> (f' a',t1+t2,c3)

instance RandomGen g => Monad (MMT n g) where
  return a = MMT $ \_ c -> (a,0,c)
  MMT f >>= m = MMT $ \g c1->
                let (g1,g2) = split g in
                case f g1 c1 of
                  (a,t1,c2) ->
                    case unMMT (m a) g2 c2 of
                      (b,t2,c3) -> (b,t1+t2,c3)

randG :: (Random a, RandomGen g) => (a,a) -> MMT n g a
randG range = MMT $ \g c -> (fst (randomR range g),0,c)

rand :: RandomGen g => (Int,Int) -> MMT n g Int
rand range = MMT $ \g c -> (fst (randomR range g),0,c)

addTime :: Time -> MMT n g ()
addTime t = MMT $ \_ c -> ((),t,c)

checkCache :: Dim n -> MMT n g (Maybe Time)
checkCache ps = MMT $ \_ cache -> ((lookup ps cache),0,cache)

addToCache :: (Dim n,Time) -> MMT n g ()
addToCache c = MMT $ \_ cache -> ((),0,c:cache)

runMMT :: RandomGen g => MMT n g a -> g -> (a,Time)
runMMT (MMT f) g = (a,t)
  where (a,t,_) = f g []

runEval :: (Dim n -> MMT n StdGen Time) -> (Dim n -> Time)
runEval eval fs = fst (runMMT (eval fs) (mkStdGen 0))
