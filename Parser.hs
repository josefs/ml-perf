{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Parser where

import Dim
import Data.Array
import Data.Char
import Control.Exception.Base

parseLine :: Read a => Sing n -> [String] -> IO (Dim n, a)
parseLine SZ [s] = return (Empty,read s)
parseLine (SS n) (w:ws) =
  do d <- catch (evaluate (read w))
            (\e@(SomeException _) ->
                  do putStrLn "Couldn't parse line ending with:"
                     putStrLn (unwords (w:ws))
                     throw e)
     (ds,t) <- parseLine n ws
     return (Dim d ds,t)

parse :: Read t => FilePath -> 
         (forall n . Array (Dim n) t -> Int -> IO a) -> IO a
parse fileName k = do
  file <- readFile fileName
  let (h:rest) = lines file
      n = read h :: Int
      f = filter nonsense rest
      len = length f
  case n of
    1 -> do let d = SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (array r dat) len
    2 -> do let d = SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (array r dat) len
    3 -> do let d = SS $ SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (array r dat) len
    4 -> do let d = SS $ SS $ SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (array r dat) len

parseSparse :: FilePath -> 
               (forall n . Array (Dim n) (Maybe Float) -> Int -> IO a) -> IO a
parseSparse fileName k = do
  file <- readFile fileName
  let (h:rest) = lines file
      n = read h :: Int
      f = filter nonsense rest
      len = length f
  case n of
    1 -> do let d = SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (mkSparse r (speedupToTimes dat)) len
    2 -> do let d = SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (mkSparse r (speedupToTimes dat)) len
    3 -> do let d = SS $ SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (mkSparse r (speedupToTimes dat)) len
    4 -> do let d = SS $ SS $ SS $ SS SZ
            dat <- mapM (parseLine d . words) f
            let r = getRange (map fst dat)
            k (mkSparse r (speedupToTimes dat)) len

mkSparse :: Ix i => (i,i) -> [(i,a)] -> Array i (Maybe a)
mkSparse bounds ls = 
  array bounds ([ (p,Nothing) | p <- (range bounds)] ++
                map (\(i,a) -> (i,Just a)) ls)

speedupToTimes :: [(a,Float)] -> [(a,Float)]
speedupToTimes = map (\(i,t) -> (i,100/t))

nonsense :: String -> Bool
nonsense l | all isSpace l = False
nonsense ('#':_) = False
nonsense _ = True

parseTimes :: Read a => FilePath -> IO [a]
parseTimes fileName = do
  file <- readFile fileName
  let (h:rest) = lines file
      f = filter nonsense rest
      n = read h :: Int
  return [ read $ head $ drop n $ words line | line <- f ]
