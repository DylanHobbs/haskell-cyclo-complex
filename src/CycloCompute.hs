module CycloCompute where

import Argon
import GitHub
import System.Process
import System.IO
import System.Exit (ExitCode (ExitSuccess))

--calculateComplexity :: String ->

-- Get all commit SHA1's
excecuteCommand :: String -> IO ()
excecuteCommand command = do
        ExitSuccess <- system command
        print "Success"

getCommitsOnRepo :: [String]
getCommitsOnRepo = do
      excecuteCommand "git rev-list >> commitList.txt"
      contents    <- readFile "commitList.txt"
      let lines = lines contents
      return lines


