module Server (mainLoop) where

import Control.Concurrent (threadDelay)
import Data.List
import Data.Text (pack, strip, unpack)
import System.Process
import TData
import Text.Regex.TDFA ((=~))

mainLoop :: String -> IO ()
mainLoop csvFile = do
  putStrLn "Hello, from server\n\n"

  info <- getInfoFromCSV csvFile
  window <- getActive
  clients <- getClients

  print window
  print clients

  putStrLn "\n\n\n"

  printInfo info
  

  let newInfo = updateClients clients "00:00:01" $ updateWindow window "00:00:01" info

  setInfoToCSV csvFile newInfo

  threadDelay 1000000 -- uses microseconds
  mainLoop csvFile

getActive :: IO String
getActive = do
  window <- readProcess "hyprctl" ["activewindow"] []
  let regex = "class: *([^ ]+)" :: String
      match = window =~ regex :: [[String]]
  case match of
    [[_, cls]] -> do
      let trimmed = unpack . strip . pack $ cls
      return trimmed
    _ -> return ""

getClients :: IO [String]
getClients = do
  clients <- readProcess "hyprctl" ["clients"] []
  let regex = "class: *([^ ]+)" :: String
      match = clients =~ regex :: [[String]]
      classNames = [unpack . strip . pack $ cls | [_, cls] <- match]
  return classNames