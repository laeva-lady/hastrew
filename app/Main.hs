module Main (main) where

import System.Directory
import TData
import Ttime
import Server (mainLoop)
import System.Environment

import Network.Socket

main :: IO ()
main = do

  soc <-socket AF_INET Stream 0
  connect soc (SockAddrUnix "$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock")
  msg <- recv soc 1024
  putStrLn msg
  
  
  -- args <- getArgs
  -- date <- getDate
  -- homeDir <- getHomeDirectory :: IO String

  -- -- TODO: have verification that the cache dir exists
  -- let cacheDir = homeDir ++ "/.cache/hastrew" :: String
  --     dailyCacheDir = cacheDir ++ "/daily/" :: String
  --     csvFile = dailyCacheDir ++ date2string date ++ ".csv" :: String

  -- if "--server" `elem` args
  --   then mainLoop csvFile
  --   else do
  --     info <- getInfoFromCSV csvFile
  --     printUsageSummary info
