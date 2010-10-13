
module Main where

import Bio.Sequence
import Data.Int
import System.Environment (getArgs)
import Data.List (intersperse, foldl')

import qualified Data.IntMap as M

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
mkplot hs ns = gnuplot (labels++ytics) $ zip hs $ map (map ((/(1e6::Double)) . fromIntegral)) ns
  where labels = "set ylabel 'size'; set xlabel 'contigs';"
        ytics  = "set format y '%.0fM'"

  -- plot '-' with lines label f -- show ints, end with e

sizes :: FilePath -> IO [Int64]
sizes f = map seqlength `fmap` readFasta f

sort :: [Int64] -> [Int64]
sort = concatMap (\(x,c) -> replicate c (fromIntegral $ negate x)) . M.toAscList . freqs


-- equivalent to  'M.fromList . map (\x->(fromIntegral $ negate x,1))', except for not blowing the stack
freqs :: [Int64] -> M.IntMap Int
freqs = foldl' ins M.empty . map fromIntegral . map negate
  where ins m x = case M.lookup x m of 
          Just v -> v `seq` M.insert x (v+1) m
          Nothing -> M.insert x 1 m
