module Sort where

import qualified Data.IntMap as M
import Bio.Sequence
import Data.List (foldl')

type SizeTable = M.IntMap Int

sizes :: FilePath -> IO SizeTable
sizes f = (freqs . map (fromIntegral . seqlength)) `fmap` readFasta f

sort :: SizeTable -> [Int]
sort = concatMap (\(x,c) -> replicate c (fromIntegral $ negate x)) . M.toAscList

-- equivalent to  'M.fromList . map (\x->(fromIntegral $ negate x,1))', 
-- except for not blowing the stack
freqs :: [Int] -> SizeTable
freqs = foldl' ins M.empty . map fromIntegral . map negate
  where ins m x = case M.lookup x m of 
          Just v -> v `seq` M.insert x (v+1) m
          Nothing -> M.insert x 1 m
