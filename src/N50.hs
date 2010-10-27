
module Main where

import System.Environment (getArgs)

import Sort

main :: IO ()
main = getArgs >>= mapM_ count

count :: FilePath -> IO ()
count f = do
	ls <- sort `fmap` sizes f
	putStr (f++"\t")
	let n50 = length $ takeWhile (<300000000) $ scanl1 (+) ls
	print (last $ take n50 ls)
