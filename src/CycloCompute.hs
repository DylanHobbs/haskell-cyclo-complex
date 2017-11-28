module CycloCompute where

import Argon
import GitHub
import System.Process
import System.IO
import System.Exit (ExitCode (ExitSuccess))

type Dir = String

calculateComplexity :: String -> Dir -> Int
calculateComplexity commit dir = length commit





