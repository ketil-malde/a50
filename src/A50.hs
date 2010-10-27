module Main where

import Data.List (intersperse)

import Gnuplot
import Blat
import Sort
import Options

main :: IO ()
main = do 
  opts <- getArgs
  ss <- mapM sizes $ inputs opts
  case estref opts of 
    "" -> do
      mkplot opts $ map (each 10) $ map ((scanl1 (+)) . sort) ss
    est -> do  
      ps <- mapM (\asm -> fmap gen_result $ runBlat (tmpdir opts) asm est) (inputs opts)
      mkplot opts $ map (each 10) $ map (scanl1 (+)) $ zipWith interleave (map sort ss) ps
  -- putStr $ zipLists fs (map ((scanl1 (+)) . sort) ss)

each :: Int -> [a] -> [a]
each _ [] = []
each n (x:xs) = x : each n (drop (n-1) xs)

zipLists :: [String] -> [[Int]] -> String
zipLists fs ss = unlines ((concat $ map ("#\t"++) fs) : go (map (++repeat 0) ([1..]:ss)))
  where go :: [[Int]] -> [String]
        go xs = let hs = map head xs
                    ts = map tail xs
                in if all (==0) $ tail hs then []
                   else (concat $ intersperse "\t" $ map myshow hs) : go ts

myshow :: Integral i => i -> String
myshow 0 = ""
myshow n = show n

mkplot :: Opt -> [[Int]] -> IO ()
mkplot o ns = gnuplot [conf,outp,labels,tics] (zip (inputs o) ns) es
  where labels = "set ylabel 'cumulative size'; set xlabel 'contig number'"
        tics  = "set format y '%.0s%c'; set format x '%.0f0'"
        conf = if null $ format o then "" else "set terminal "++format o
        outp = if null $ outfile o then "" else "set out '"++outfile o++"'"
        es   = map read $ expect o
                  
