--  blat DB QRY OUT -t= -d= -minScore= -extendThroughN
--  blat P_2010_10_11_10_15_26_runAssembly/454AllContigs.fna ../EST/lakselus.fasta -t=dna -q=rna tmp.psl -minScore=50

module Blat (module Bio.Alignment.PSL, runBlat, gen_result, interleave) where

import Bio.Alignment.PSL
import Control.Arrow (second)
import Data.List (sortBy)
import System.Directory (doesFileExist, findExecutable)
import Control.Monad (when)
import System.Process (runCommand, waitForProcess) 
import System.Exit (ExitCode(..))

blat_cmd :: String
blat_cmd = "blat -t=dna -q=rna -minScore=50 -extendThroughN "

-- run blat
runBlat :: FilePath -> FilePath -> FilePath -> IO [PSL]
runBlat tmpdir asm ests = do
  let pslfile = tmpdir++"/"++basename asm++"_vs_"++basename ests++".psl"
      basename = reverse . takeWhile (/='/') . reverse
  dfe <- doesFileExist pslfile
  when (not dfe) $ do
    fe <- findExecutable "blat"
    when (fe == Nothing) $ error "Couldn't find the 'blat' executable - aborting"
    blat <- runCommand $ unwords [blat_cmd,asm,ests,pslfile]
    e <- waitForProcess blat
    case e of ExitSuccess -> return ()
              _ -> error $ show e
  readPSL pslfile

-- this will only get you the sequences that have a match!
gen_result :: [PSL] -> [(Int,Int)]
gen_result = coverage . order . pslbest

interleave :: Integral i => [i] -> [(i,i)] -> [i]
interleave sz [] = map (const 0) sz
interleave (sz:szs) covs@((s1,c1):cs) 
  | sz > s1   = 0 : interleave szs covs
  | sz == s1  = c1 : interleave szs cs

-- if this fails, something is wrong with my assumptions!
  | otherwise = error ("interleave failed: "++show sz++" "++show (s1,c1))
interleave [] rs = error ("interleave failed - leftover contigs: "++show (take 10 rs)++"...")

coverage :: [PSL] -> [(Int,Int)]
coverage = map (second (sum . map (sum . blocksizes))) . tgroup
  where tgroup [] = []
        tgroup (p1:ps) = let (this,rest) = span ((tname p1 ==) . tname) ps
                         in (tsize p1, p1:this) : tgroup rest

order :: [PSL] -> [PSL]
order = sortBy (comparing (\c -> (negate $ tsize c,tname c)))

pslbest :: [PSL] -> [PSL]
pslbest = map (last . sortBy (comparing match)) . qgroup
  where 
    qgroup [] = []
    qgroup (p1:ps) = let (this,rest) = span ((qname p1 ==) . qname) ps
                     in (p1:this) : qgroup rest

comparing :: (Ord a) => (t -> a) -> t -> t -> Ordering
comparing f a b = compare (f a) (f b)