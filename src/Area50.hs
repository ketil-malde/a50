
module Main where

import Bio.Sequence
import Data.Int
import System.Environment (getArgs)
import Data.List (intersperse)


main :: IO ()
main = do 
  fs <- getArgs
  ss <- mapM (\f -> sort `fmap` sizes f) fs
  putStr $ zipLists fs (map (scanl1 (+)) ss)

zipLists :: [String] -> [[Int64]] -> String
zipLists fs ss = unlines ((concat $ map ('#':'\t':) fs) : go (map (++repeat 0) ([1..]:ss)))
  where go :: [[Int64]] -> [String]
        go xs = let hs = map head xs 
                    ts = map tail xs
                in if all (==0) $ tail hs then []
                   else (concat $ intersperse "\t" $ map myshow hs) : go ts

myshow 0 = ""
myshow n = show n


gnuplot :: [String] -> [[Int64]] -> IO ()
gnuplot = undefined
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