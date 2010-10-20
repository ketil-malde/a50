{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import System.Console.CmdArgs
import Control.Monad (when)
import System.Directory (getTemporaryDirectory)
import Foreign (unsafePerformIO)

tmpdefault :: FilePath
tmpdefault = unsafePerformIO getTemporaryDirectory

data Opt = Opt { outfile  :: FilePath
               , format :: String
               , expect   :: [String]
               , inputs   :: [FilePath]
               , estref   :: FilePath
               , tmpdir   :: FilePath
               } deriving (Typeable, Data, Show, Eq)

myopt :: Opt
myopt = Opt 
  { outfile = def &= help "Output file, if applicable" &= typFile
  , format  = def &= help "Gnuplot output format (a.k.a. 'terminal')"
  , expect  = []  &= help "Expected genome size"
  , inputs  = def &= args &= typFile
  , estref  = def &= help "Reference transcripts" &= typFile &= name "E"
  , tmpdir  = tmpdefault &= help "Set temporary directory" &= typDir &= name "T"
  } &= summary "a50 - compare genome assemblies" &= program "a50"

getArgs :: IO Opt
getArgs = do
  o <- setOutputFormat `fmap` cmdArgs myopt
  when (null $ inputs o) $ error "Please specify one or more input files!"
  return o
  
setOutputFormat :: Opt -> Opt
setOutputFormat o 
  | null (format o) && null (outfile o) = o
  | null (format o)                     = o { format = determineFormat $ outfile o }
  | null (outfile o)                    = error "Please specify an output file (-o) when you specify format."
  | otherwise       = o
    where 
      determineFormat fp =
        let ext = reverse . takeWhile (/='.') . reverse
        in case ext fp of 
          "png" -> "png"
          "jpg" -> "jpg"
          "ps"  -> "postscript eps"
          "svg" -> "svg"
          "pdf" -> "pdf"
          _ -> error "Couldn't determine format from output file name.\nPlease specify format with -f."
