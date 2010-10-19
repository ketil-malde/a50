module Main where

import Bio.Sequence
import Data.List (intersperse, foldl')

import qualified Data.IntMap as M

import Gnuplot
import Blat
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
                  
sizes :: FilePath -> IO [Int]
sizes f = map (fromIntegral . seqlength) `fmap` readFasta f

sort :: [Int] -> [Int]
sort = concatMap (\(x,c) -> replicate c (fromIntegral $ negate x)) . M.toAscList . freqs

-- equivalent to  'M.fromList . map (\x->(fromIntegral $ negate x,1))', 
-- except for not blowing the stack
freqs :: [Int] -> M.IntMap Int
freqs = foldl' ins M.empty . map fromIntegral . map negate
  where ins m x = case M.lookup x m of 
          Just v -> v `seq` M.insert x (v+1) m
          Nothing -> M.insert x 1 m
