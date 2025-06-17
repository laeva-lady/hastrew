module Main (main) where

import System.Directory
import TData
import Ttime
import Server (mainLoop)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  date <- getDate
  homeDir <- getHomeDirectory :: IO String
  let cacheDir = homeDir ++ "/.cache/hastrew" :: String
      dailyCacheDir = cacheDir ++ "/daily/" :: String
      csvFile = dailyCacheDir ++ date2string date ++ ".csv" :: String

  if "--server" `elem` args 
    then mainLoop csvFile
    else if "--client" `elem` args 
      then do
        info <- getInfoFromCSV csvFile
        printUsageSummary info
      else 
        putStrLn "No arguments provided"