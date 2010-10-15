{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import System.Console.CmdArgs
import Control.Monad (when)

version :: String
version = "area50 v0 - graphical comparison of genome assemblies, ©2010 Ketil Malde."

data Opt = Opt { outfile  :: FilePath
               , terminal :: String
               , expect   :: [String]
               , inputs   :: [FilePath]
               , estref   :: FilePath
               } deriving (Typeable, Data, Show, Eq)

myopt :: Opt
myopt = Opt 
  { outfile = def &= help "Output file, if applicable" &= typFile
  , terminal= def &= help "Gnuplot output format ('terminal')"
  , expect  = []  &= help "Expected genome size"
  , inputs  = def &= args &= typFile
  , estref  = def &= help "Reference transcripts" &= typFile
  }

getArgs :: IO Opt
getArgs = do
  o <- cmdArgs myopt
  when (null $ inputs o) $ error "Please specify one or more input files!"
  -- todo: check that output is specified with terminal!
  return o