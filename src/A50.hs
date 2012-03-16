module Main where

import Data.List (intersperse)

import Gnuplot
import Blat
import Sort    (sizes, sort, SizeTable)
import Options (Opt(..), getArgs)
import Control.Monad (when)

main :: IO ()
main = do 
  opts <- getArgs
  ss <- mapM sizes $ inputs opts
  case estref opts of 
    "" -> do
      when (format opts /= "plot") $ n50 (map read $ expect opts) $ zip (inputs opts) ss
      mkplot opts $ map (each 10) $ map ((scanl1 (+)) . sort) ss
    est -> do  
      ps <- mapM (\asm -> fmap gen_result $ runBlat (tmpdir opts) asm est) (inputs opts)
      mkplot opts $ map (each 10) $ map (scanl1 (+)) $ zipWith interleave (map sort ss) ps
  -- putStr $ zipLists fs (map ((scanl1 (+)) . sort) ss)

n50 :: [Int] -> [(FilePath,SizeTable)] -> IO ()
n50 [e] iss = do
  let genOut (f,st) = putStrLn (f ++ "\t"++go [e`div`4,e`div`2,3*e`div`4] (scanl1 (+) $ sort st))
      go (e0:es) (z1:z2:zs) = 
        if z2 > e0 then show (z2-z1)++"\t"++go es (z1:z2:zs)
        else go (e0:es) (z2:zs)
      go [] (z1:z2:zs) = z1 `seq` go [] (z2:zs)
      go es [z1] = concatMap (const "-\t") es ++ show z1++" total size"
      go _ []  = error "shouldn't happen"
  putStrLn ("Assembly \tn25\tn50\tn75\t\testimated size="++show e)
  mapM_ genOut iss
n50 _ _ = return ()

each :: Int -> [a] -> [(Int,a)]
each k = go 0
  where
    go _ [] = []
    go i (x:xs) = (i,x) : go (i+k) (drop (k-1) xs)

zipLists :: [String] -> [[Int]] -> String
zipLists fs ss = unlines ((concat $ map ("#\t"++) fs) : go (map (++repeat 0) ([1..]:ss)))
  where go :: [[Int]] -> [String]
        go xs = let hs = map head xs
                    ts = map tail xs
                in if all (==0) $ tail hs then []
                   else (concat $ intersperse "\t" $ map myshow hs) : go ts

myshow :: (Integral i, Show i) => i -> String
myshow 0 = ""
myshow n = show n

mkplot :: Opt -> [[(Int,Int)]] -> IO ()
mkplot o ns = let my_out = if format o == "plot" then gnudat else gnuplot
              in my_out [conf,outp,labels,tics] (zip (inputs o) ns) es
  where labels = "set ylabel 'cumulative size'; set xlabel 'contig number'"
        tics  = "set format y '%.1s%c'; set format x '%.0f'"
        conf = if (null $ format o) || format o == "plot" then "" else "set terminal "++format o
        outp = if null $ outfile o then "" else "set out '"++outfile o++"'"
        es   = map read $ expect o
                  
