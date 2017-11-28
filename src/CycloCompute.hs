module CycloCompute where

import Argon
import GitHub
import System.Process
import System.IO
import System.Exit (ExitCode (ExitSuccess))
import SuperHelper5000
import Control.Monad
import Data.Aeson

type Dir = String

--excecuteC :: String -> IO ()
--excecuteC command = do
--        ExitSuccess <- system command
--        print $ "Command: |" ++ command ++ "| executed successfully"

--calculateComplexity :: String -> Dir -> Int
--calculateComplexity commit dir = do
--                let a = excecuteCommand_ ("argon " ++ dir ++ " >> " ++ dir ++ "/output")
--                let contents >>= readFile $ dir ++ "/output"
--                l <- lines contents
--                mapM_ putStrLn l
--                let x = length l
--                return x

--caclculateComplexity :: String -> Dir -> Int
--caclculateComplexity commit dir = do
--                  let a = analyze defaultConfig dir
--                  let t = (liftM snd) a
--                  let v = toJSON t


calculateComplexity :: String -> Dir -> Int
calculateComplexity commit dir = length commit





