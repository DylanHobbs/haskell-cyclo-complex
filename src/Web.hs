{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web where

import Data.Text.Lazy
import Web.Scotty as S
import System.Environment
import Lib
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Control.Concurrent
import Control.Exception

beamMeUpScotty :: IO ()
beamMeUpScotty = S.scotty 3000 $ do
  S.get "/doTheThing/:host/:port/:n/:user/:repo" $
    do host <- param "host"
       port <- param "port"
       n    <- param "n"
       user <- param "user"
       repo <- param "repo"

       let repoPath = "https://github.com/" ++ user ++ "/" ++ repo
       let portNum = read port :: Int
       let n_num = read n :: Int
       let startPort = portNum - 2

--       liftIO $ putStrLn ("Host: " ++ host ++ " Port: " ++ port ++ " N:" ++ n ++ " User:" ++ user ++ " Repo: " ++ repo)
       liftIO $ forkIO $ startNWorkers n_num host startPort repoPath `finally` startManager host port n repoPath
--       liftIO $ startManager host port n repoPath
       html "Hello world"

  S.get "/" $
    html "Hello World"

{-|
  Function to start N workers from the scotty call.
  This will be changed to start remote docker nodes using
  docker compose files. But to demonstate distributed prog
  it'll just spawn n workers on given host with decremented
  port numbers
-}
-- N -> StartPort -> host -> port -> RepoPath
startNWorkers :: Int -> String -> Int -> String -> IO ()
startNWorkers 0 _ _ _ = print "done"
startNWorkers n host port repoPath = do
              forkIO $ startWorker host (show port) repoPath
              startNWorkers (n-1) host (port - 1) repoPath

--startManagers :: String -> String -> String -> String
--startManagers =