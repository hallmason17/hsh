module Main where

import Control.Exception (IOException, handle)
import Control.Monad (forever)
import Data.Text
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.Process (callProcess)

data Action = Command Text [Text]

handler :: IOException -> IO ()
handler e = putStrLn "Invalid command, try again."

logCommandToFile :: Text -> IO ()
logCommandToFile cmd = TIO.appendFile ".hsh_history" (cmd `append` pack "\n")

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
  putStr "\ESC[93m~~> \ESC[0m"
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
