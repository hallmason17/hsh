module Main where

import Control.Exception (IOException, handle, try)
import Control.Monad (forever)
import Data.List.Split (splitOn)
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text.IO as TIO (appendFile, getLine)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Directory (changeWorkingDirectory)
import System.Process (callProcess)

data Action = Command Text [Text]

handler :: IOException -> IO ()
handler _ = putStrLn "Invalid command, try again."

readHandler :: IOError -> IO String
readHandler e
  | isDoesNotExistError e = do return ""
  | otherwise = do putStrLn "An error occurred."; return ""

getCurrentBranchIfExist :: FilePath -> IO String
getCurrentBranchIfExist cwd = do
  let gitDir = cwd ++ "/.git/HEAD"
  contents <- handle readHandler $ readFile gitDir
  return $ head $ splitOn "\n" $ last $ splitOn "/" contents

logCommandToFile :: Text -> IO ()
logCommandToFile cmd = do
  homeDir <- getEnv "HOME"
  TIO.appendFile (homeDir ++ "/.hsh_history") (cmd `append` pack "\n")

getCommandAndArgs :: String -> (String, [String])
getCommandAndArgs line = (cmd, args)
  where
    wrds = words line
    cmd = head wrds
    args = tail wrds

maybeRunCommand :: String -> [String] -> IO ()
maybeRunCommand cmd args = do
  case cmd of
    "cd" -> changeWorkingDirectory $ head args
    "exit" -> exitSuccess
    _ -> handle handler $ callProcess cmd args

prompt :: IO Text
prompt = do
  maybeBranch <- try (getCurrentBranchIfExist ".") :: IO (Either IOException String)
  case maybeBranch of
    Left _ -> putStr "\ESC[93m> \ESC[0m"
    Right "" -> putStr "\ESC[93m> \ESC[0m"
    Right branch -> putStr ("\ESC[93m> \ESC[30m" ++ "(" ++ branch ++ ")" ++ "\ESC[0m")
  hFlush stdout
  TIO.getLine

promptLoop :: IO ()
promptLoop = forever $ do
  line <- prompt
  let (cmd, args) = getCommandAndArgs (unpack line)
  logCommandToFile line
  maybeRunCommand cmd args

main :: IO ()
main = do
  putStrLn "\ESC[36mWelcome to hsh!"
  promptLoop
