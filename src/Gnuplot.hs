module Gnuplot where    

import System.Process
import System.Exit
import System.IO
import Data.List (intersperse)
import Data.Maybe (maybeToList)

import Control.Monad (when)
import System.Directory (findExecutable)
import Control.Concurrent (forkIO)

gnuplot :: (Show i, Num i) => [String] -> [(String,[(Int,i)])] -> Maybe Double -> IO ()
gnuplot preamble cols hlines = do
  fe <- findExecutable "gnuplot"
  when (fe == Nothing) $ error "Couldn't find the 'gnuplot' executable - aborting"
  (i,o,e,p) <- runInteractiveCommand "gnuplot -persist"
  _ <- forkIO (hGetContents o >>= hPutStr stdout)
  -- ugly hack to limit error output (stop after first error)
  _ <- forkIO $ do 
    let go [] = return ()
        go (l1:rest) = do putStrLn l1
                          if ('^' `elem` l1) 
                            then case rest of (l2:_) -> putStrLn l2 
                                              _ -> return ()
                            else go rest
    go =<< fmap lines (hGetContents e)
  genplot i preamble cols hlines
  hClose i
  x <- waitForProcess p
  case x of ExitSuccess ->  return ()
            ExitFailure j -> hPutStrLn stderr (errmsg++show j) >> return ()
    where errmsg = "'gnuplot' failed with exit code "

gnudat :: (Show i, Num i) => [String] -> [(String,[(Int,i)])] -> Maybe Double -> IO ()
gnudat = genplot stdout

-- | Write gnuplot instructions to a handle (todo: make pure)
genplot :: (Show i, Num i) => Handle -> [String] -> [(String,[(Int,i)])] -> Maybe Double -> IO ()
genplot i preamble cols hlines = do
  hPutStr i $ unlines $ preamble
  let show' (x,y) = show x ++ "\t" ++ show y
  hPutStrLn i (mkplots cols ++ concatMap ((',':) . show) (maybeToList hlines))
  mapM_ (\col -> do {hPutStr i . unlines . map show' . snd $ col; hPutStrLn i "e"}) cols

-- | generate the plot command
mkplots :: [(String,[a])] -> String
mkplots cs = "plot " ++ (concat . intersperse "," . map mkp $ cs)
  where mkp :: (String,a) -> String
        mkp (s,_) = "'-' with lines title '"++s++"'"

