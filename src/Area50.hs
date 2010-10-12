
module Main where

import Bio.Sequence
import Data.Int
import System.Environment (getArgs)
import Data.List (intersperse)

import Gnuplot

main :: IO ()
main = do 
  fs <- getArgs
  s1 <- mapM sizes fs
  let ss = (map ((scanl1 (+)) . sort) s1)
  mkplot fs $ map (each 10) ss
  -- putStr $ zipLists fs (map ((scanl1 (+)) . sort) ss)

each :: Int -> [a] -> [a]
each _ [] = []
each n (x:xs) = x : each n (drop (n-1) xs)

zipLists :: [String] -> [[Int64]] -> String
zipLists fs ss = unlines ((concat $ map ("#\t"++) fs) : go (map (++repeat 0) ([1..]:ss)))
  where go :: [[Int64]] -> [String]
        go xs = let hs = map head xs 
                    ts = map tail xs
                in if all (==0) $ tail hs then []
                   else (concat $ intersperse "\t" $ map myshow hs) : go ts

myshow :: Integral i => i -> String
myshow 0 = ""
myshow n = show n

mkplot :: [String] -> [[Int64]] -> IO ()
mkplot hs ns = gnuplot "" $ zip hs ns

  -- plot '-' with lines label f -- show ints, end with e

sizes :: FilePath -> IO [Int64]
sizes f = map seqlength `fmap` readFasta f

sort :: [Int64] -> [Int64]
sort []     = []
sort [x]    = [x]
sort (x:xs) = sort gt ++ x:eq ++ sort lt
  where lt = filter (< x) xs
        eq = filter (== x) xs
        gt = filter (> x) xs