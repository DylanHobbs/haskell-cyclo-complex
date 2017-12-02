module SuperHelper5000
          ( excecuteCommand_
          , excecuteCommand
          ) where

import System.Process
import System.IO
import System.Exit

excecuteCommand_ :: String -> IO ()
excecuteCommand_ command = do
        e <- system command
        case e of
          ExitSuccess -> print $ "Command: |" ++ command ++ "| executed successfully"
          _           -> print $ "Command: |" ++ command ++ "| failed"

excecuteCommand :: String -> String -> IO Handle
excecuteCommand command dir = do
        (Just hin, Just hout, Just herr, _) <- createProcess (proc "ls" []){ cwd = Just dir,std_out = CreatePipe }
        return hout