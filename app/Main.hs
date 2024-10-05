module Main where

import Control.Exception (IOException, handle, try)
import Control.Monad (forever)
import Data.List.Split (splitOn)
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text.IO as TIO
import System.Environment (getEnv)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process (callProcess)

data Action = Command Text [Text]

handler :: IOException -> IO ()
handler _ = putStrLn "Invalid command, try again."

readHandler :: IOError -> IO String
readHandler e
  | isDoesNotExistError e = do putStrLn "No git repository found in this directory."; return ""
  | otherwise = do putStrLn "An error occurred."; return ""

getCurrentBranchIfExist :: FilePath -> IO String
getCurrentBranchIfExist cwd = do
  let gitDir = cwd ++ "/.git/HEAD"
  contents <- handle readHandler $ readFile gitDir
  return $ Prelude.head $ splitOn "\n" $ Prelude.last $ splitOn "/" contents

logCommandToFile :: Text -> IO ()
logCommandToFile cmd = do
  homeDir <- getEnv "HOME"
  TIO.appendFile (homeDir ++ "/.hsh_history") (cmd `append` pack "\n")

getCommandAndArgs :: String -> (String, [String])
getCommandAndArgs line = (cmd, args)
  where
    wrds = Prelude.words line
    cmd = Prelude.head wrds
    args = Prelude.tail wrds

maybeRunCommand :: String -> [String] -> IO ()
maybeRunCommand cmd args = do
  handle handler $ callProcess cmd args

prompt :: IO Text
prompt = do
  maybeBranch <- try (getCurrentBranchIfExist ".") :: IO (Either IOException String)
  case maybeBranch of
    Left _ -> putStr "\ESC[93m~~> \ESC[0m"
    Right branch -> putStr ("\ESC[93m~~> \ESC[30m" ++ "(" ++ branch ++ ")" ++ "\ESC[0m")
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
