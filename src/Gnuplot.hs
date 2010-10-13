module Gnuplot where    

import System.Process
import System.Exit
import System.IO
import Data.List (intersperse)

gnuplot :: Num i => [String] -> [(String,[i])] -> IO ()
gnuplot preamble cols = do
  -- putStrLn $ unlines $ map fst cols
  (i,o,e,p) <- runInteractiveCommand "gnuplot -persist"
  hPutStr i $ unlines $ preamble
  hPutStrLn i $ mkplots cols
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

