{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# CPP #-}

-- | use-haskell
-- The purpose of this project is to provide a baseline demonstration of the use of cloudhaskell in the context of the
-- code complexity measurement individual programming project. The cloud haskell platform provides an elegant set of
-- features that support the construction of a wide variety of multi-node distributed systems commuinication
-- architectures. A simple message passing abstraction forms the basis of all communication.
--
-- This project provides a command line switch for starting the application in master or worker mode. It is implemented
-- using the work-pushing pattern described in http://www.well-typed.com/blog/71/. Comments below describe how it
-- operates. A docker-compose.yml file is provided that supports the launching of a master and set of workers.

module Lib
    (
      startWorker
    , startManager
    ) where

-- These imports are required for Cloud Haskell
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)
import           PrimeFactors
import           System.Environment                                 (getArgs)
import           System.Exit

import Argon
import GitHub
import System.Process
import System.IO
import System.Exit (ExitCode)
import CycloCompute
import SuperHelper5000

-- this is the work we get workers to do. It could be anything we want. To keep things simple, we'll calculate the
-- number of prime factors for the integer passed.
doWork :: String -> String -> Int
doWork = calculateComplexity

-- | worker function.
-- This is the function that is called to launch a worker. It loops forever, asking for work, reading its message queue
-- and sending the result of runnning numPrimeFactors on the message content (an integer).
worker :: ( ProcessId   -- The processid of the manager (where we send the results of our work)
          , ProcessId    -- the process id of the work queue (where we get our work from)
          , String)      -- Repo path for cloning
         -> Process ()
worker (manager, workQueue, repoPath) = do
    us <- getSelfPid                          -- get our process identifier
    let temp_dir = "tmp-worker" ++ drop 16 (show us)   -- create a scratch dir for worker work

    liftIO $ putStrLn $ "Clonging worker copy of repo: " ++ repoPath
    liftIO $ excecuteCommand_ ("git clone " ++ repoPath ++ " " ++ temp_dir)
    liftIO $ putStrLn $ "Starting worker: " ++ show us
    go us temp_dir
  where
    go :: ProcessId -> String -> Process ()
    go us dir = do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \n  -> do
            liftIO $ putStrLn $ "[Node " ++ show us ++ "] given work: " ++ n
            send manager (doWork n dir)
            liftIO $ putStrLn $ "[Node " ++ show us ++ "] finished work."
            go us dir-- note the recursion this function is called again!
        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            liftIO $ putStrLn "Performing cleanup"
            liftIO $ excecuteCommand_ ("rm -rf " ++ "tmp-worker" ++ drop 16 (show us))
            return ()
        ]

remotable ['worker] -- this makes the worker function executable on a remote node

manager :: Integer    -- The number range we wish to generate work for (there will be n work packages)
        -> [NodeId]   -- The set of cloud haskell nodes we will initalise as workers
        -> [String]   -- List of sha commits
        -> String     -- Repo name for remote cloning
        -> Process Integer
manager n workers list repoPath = do
  us <- getSelfPid

  -- first, we create a thread that generates the work tasks in response to workers
  -- requesting work.
  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
--    forM_ [1 .. n] $ \m -> do
--      pid <- expect   -- await a message from a free worker asking for work
--      send pid m     -- send them work
    forM_ list $ \item -> do
      pid <- expect
      send pid item

    -- Once all the work is done tell the workers to terminate. We do this by sending every worker who sends a message
    -- to us a null content: () . We do this only after we have distributed all the work in the forM_ loop above. Note
    -- the indentiation - this is part of the workQueue do block.
    forever $ do
      pid <- expect
      send pid ()

  -- Next, start worker processes on the given cloud haskell nodes. These will start
  -- asking for work from the workQueue thread immediately.
  forM_ workers $ \ nid -> spawn nid ($(mkClosure 'worker) (us, workQueue, repoPath))
  liftIO $ putStrLn "[Manager] Workers spawned"
  -- wait for all the results from the workers and return the sum total. Look at the implementation, whcih is not simply
  -- summing integer values, but instead is expecting results from workers.
  sumIntegers (length list)

-- note how this function works: initialised with n, the number range we started the program with, it calls itself
-- recursively, decrementing the integer passed until it finally returns the accumulated value in go:acc. Thus, it will
-- be called n times, consuming n messages from the message queue, corresponding to the n messages sent by workers to
-- the manager message queue.
sumIntegers :: Int -> Process Integer
sumIntegers = go 0
  where
    go :: Integer -> Int -> Process Integer
    go !acc 0 = return acc
    go !acc n = do
      m <- expect
      go (acc + m) (n - 1)

--sumIntegers :: Int -> Process String
--sumIntegers = go "start:"
--  where
--    go :: String -> Int -> Process String
--    go !acc 0 = return acc
--    go !acc n = do
--      m <- expect
--      go (acc ++ " " ++ m) (n - 1)

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable


startManager :: String -> String -> String -> String -> IO ()
startManager host port n repoPath = do
      excecuteCommand_ ("git clone " ++ repoPath ++ " tmp-manager")
      excecuteCommand_ "git --git-dir tmp-manager/.git rev-list master >> commitList.txt"
      putStrLn "Cleaning up manager temp"
      excecuteCommand_ "rm -rf tmp-manager"
      contents    <- readFile "commitList.txt"
      print $ "Contents: " ++ contents
      putStrLn "Cleaning up commitList"
      excecuteCommand_ "rm -rf commitList.txt"
      let l = lines contents
      print "List:"
      mapM_ print l
      putStrLn "Starting Node as Manager"
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        result <- manager (read n) workers l repoPath
        liftIO $ print "Result:"
        liftIO $ print result

startWorker :: String -> String -> String -> IO ()
startWorker host port repoPath = do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend

-- | This is the entrypoint for the program. We deal with program arguments and launch up the cloud haskell code from
---- here.
--someFunc :: IO ()
--someFunc = do
--  args <- getArgs
--
--  case args of
--    ["manager", host, port, n, repoPath] -> do
--      excecuteCommand_ ("git clone " ++ repoPath ++ " tmp-manager")
--      excecuteCommand_ "git --git-dir tmp-manager/.git rev-list master >> commitList.txt"
--      putStrLn "Cleaning up manager temp"
--      excecuteCommand_ "rm -rf tmp-manager"
--      contents    <- readFile "commitList.txt"
--      let l = lines contents
--      print "List:"
--      mapM_ print l
--
--      putStrLn "Starting Node as Manager"
--      backend <- initializeBackend host port rtable
--      startMaster backend $ \workers -> do
--        result <- manager (read n) workers l repoPath
--        liftIO $ print result
--    ["worker", host, port] -> do
--      putStrLn "Starting Node as Worker"
--      backend <- initializeBackend host port rtable
--      startSlave backend
--    _ -> putStrLn "Bad parameters"
--
--
--  -- create a cloudhaskell node, which must be initialised with a network transport
--  -- Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
--  -- node <- newLocalNode transport initRemoteTable
--
--  -- runProcess node $ do
--  --   us <- getSelfNode
--  --   _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
--  --   pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
--  --   liftIO $ threadDelay 2000000
