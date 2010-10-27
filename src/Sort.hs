module Sort where

import qualified Data.IntMap as M
import Bio.Sequence
import Data.List (foldl')

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
