module Server (mainLoop) where

import Control.Concurrent (threadDelay)
import Data.Text (pack, strip, unpack)
import System.Process
import TData
import Text.Regex.TDFA ((=~))

mainLoop :: String -> IO ()
mainLoop csvFile = do
  info <- getInfoFromCSV csvFile
  window <- getActive
  clients <- getClients
  setInfoToCSV csvFile $ updateClients clients "00:00:01" $ updateWindow window "00:00:01" info
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
