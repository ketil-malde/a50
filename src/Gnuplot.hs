module Gnuplot where    

import System.Process
import System.Exit
import System.IO
import Data.List (intersperse)

import Control.Monad (when)
import System.Directory (findExecutable)

gnuplot :: Num i => [String] -> [(String,[i])] -> [Int] -> IO ()
gnuplot preamble cols hlines = do
  fe <- findExecutable "gnuplot"
  when (fe == Nothing) $ error "Couldn't find the 'gnuplot' executable - aborting"
  -- putStrLn $ unlines $ map fst cols
  (i,o,e,p) <- runInteractiveCommand "gnuplot -persist"
  hPutStr i $ unlines $ preamble
  hPutStrLn i (mkplots cols ++ concatMap ((',':) . show) hlines)
  mapM_ (\col -> do {hPutStr i . unlines . map show . snd $ col; hPutStrLn i "e"}) cols
  hClose i
  x <- waitForProcess p
  hGetContents o >>= hPutStr stderr
  hGetContents e >>= hPutStr stderr
  case x of ExitSuccess ->  return ()
            ExitFailure j -> hPutStr stderr (errmsg++show j) >> return ()
    where errmsg = "'gnuplot' failed with exit code "

-- | generate the plot command
mkplots :: [(String,[a])] -> String
mkplots cs = "plot " ++ (concat . intersperse "," . map mkp $ cs)
  where mkp :: (String,a) -> String
        mkp (s,_) = "'-' with lines title '"++s++"'"

