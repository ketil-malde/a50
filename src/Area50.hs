module Main where

import Bio.Sequence
import Data.Int
import Data.List (intersperse, foldl')

import qualified Data.IntMap as M

import Gnuplot
import Options

main :: IO ()
main = do 
  opts <- getArgs
  s1 <- mapM sizes $ inputs opts
  let ss = (map ((scanl1 (+)) . sort) s1)
  mkplot opts $ map (each 10) ss
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

mkplot :: Opt -> [[Int64]] -> IO ()
mkplot o ns = gnuplot [conf,outp,labels,tics] (zip (inputs o) ns) es
  where labels = "set ylabel 'size'; set xlabel 'contigs'"
        tics  = "set format y '%.0s%c'; set format x '%.0f0'"
        conf = if null $ terminal o then "" else "set terminal "++terminal o
        outp = if null $ outfile o then "" else "set out '"++outfile o++"'"
        es   = map read $ expect o
                  
sizes :: FilePath -> IO [Int64]
sizes f = map seqlength `fmap` readFasta f

sort :: [Int64] -> [Int64]
sort = concatMap (\(x,c) -> replicate c (fromIntegral $ negate x)) . M.toAscList . freqs

-- equivalent to  'M.fromList . map (\x->(fromIntegral $ negate x,1))', 
-- except for not blowing the stack
freqs :: [Int64] -> M.IntMap Int
freqs = foldl' ins M.empty . map fromIntegral . map negate
  where ins m x = case M.lookup x m of 
          Just v -> v `seq` M.insert x (v+1) m
          Nothing -> M.insert x 1 m
