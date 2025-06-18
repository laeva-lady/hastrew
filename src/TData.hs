module TData
  ( ProcessInfo (..),
    getInfoFromCSV,
    printInfo,
    printUsageSummary,
    setInfoToCSV,
    showInfo,
    showInfos,
    updateInfo,
    updateClients,
    updateWindow,
  )
where

import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory
import Text.Printf (printf)
import Ytils

data ProcessInfo = ProcessInfo
  { windowName :: String,
    time :: String,
    activeTime :: String
  }
  deriving (Show, Eq)

updateWindow :: String -> String -> [ProcessInfo] -> [ProcessInfo]
updateWindow windowname activetimetoadd infos =
  let (found, updated) =
        foldr
          ( \info (f, acc) ->
              if windowName info == windowname
                then (True, updateInfo info activetimetoadd "00:00:00" : acc)
                else (f, info : acc)
          )
          (False, [])
          infos
   in if found
        then updated
        else ProcessInfo windowname "00:00:00" activetimetoadd : updated

updateClients :: [String] -> String -> [ProcessInfo] -> [ProcessInfo]
updateClients clients usagetimetoadd infos =
  let (updated, namesFound) =
        foldr
          ( \info (acc, seen) ->
              if windowName info `elem` clients
                then (updateInfo info "00:00:00" usagetimetoadd : acc, windowName info : seen)
                else (info : acc, seen)
          )
          ([], [])
          infos
      -- Add new clients not already found
      newEntries = [ProcessInfo c usagetimetoadd "00:00:00" | c <- clients, c `notElem` namesFound]
   in updated ++ newEntries

updateInfo :: ProcessInfo -> String -> String -> ProcessInfo
updateInfo info activetimetoadd usagetimetoadd =
  let newTime = addTimes (time info) usagetimetoadd
      newActiveTime = addTimes (activeTime info) activetimetoadd
   in ProcessInfo (windowName info) newTime newActiveTime

setInfoToCSV :: String -> [ProcessInfo] -> IO ()
setInfoToCSV pathtocsv info = do
  writeFile pathtocsv $ intercalate "\n" $ map showInfo info

getInfoFromCSV :: String -> IO [ProcessInfo]
getInfoFromCSV pathtocsv = do
  exists <- doesFileExist pathtocsv
  if not exists
    then do
      writeFile pathtocsv ""
      return []
    else do
      contents <- readFile pathtocsv
      let line_contents = lines contents
          parsed = mapMaybe parseLine line_contents
      length parsed `seq` return parsed

parseLine :: String -> Maybe ProcessInfo
parseLine line =
  case wordsWhen (== ',') line of
    [w, t, a] -> Just $ ProcessInfo w t a
    _ -> Nothing

showInfo :: ProcessInfo -> String
showInfo info =
  Ytils.unwords ',' [windowName info, time info, activeTime info]

showInfos :: [ProcessInfo] -> String
showInfos infos = intercalate "\n" $ map showInfo infos

printInfo :: [ProcessInfo] -> IO ()
printInfo info = do
  putStrLn "Window Name,Time,Active Time"
  forM_ info $ \i -> do
    putStrLn $ Prelude.unwords [windowName i, time i, activeTime i]

printUsageSummary :: [ProcessInfo] -> IO ()
printUsageSummary entries = do
  let sumTime func = diffTimeToStr . sum $ map (fromMaybe 0 . parseTimeStr . func) entries
  let totalActive = sumTime activeTime
      totalUsage = sumTime time

  putStrLn ""
  putStrLn $ "Today's Active Usage\t" ++ totalActive
  putStrLn $ "Today's Total Usage\t" ++ totalUsage
  putStrLn $ red ++ replicate 60 '-' ++ reset
  printf "%s%-30s%15s%15s%s\n" yellow "App" "App's lifetime" "Active Time" reset
  putStrLn $ red ++ replicate 60 '-' ++ reset

  mapM_ printEntry entries
  where
    red = "\x1b[31m"
    yellow = "\x1b[33m"
    green = "\x1b[32m"
    blue = "\x1b[34m"
    reset = "\x1b[0m"
    printEntry (ProcessInfo name time active) =
      printf
        "%s%-30s%s%s%15s%s%s%15s%s\n"
        blue
        name
        reset
        green
        time
        reset
        green
        active
        reset
