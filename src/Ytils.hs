module Ytils
  ( wordsWhen,
    Ytils.unwords,
    addTimes,
    parseTimeStr,
    diffTimeToStr
  )
where

import Data.Maybe (fromMaybe)
import Data.Time

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

unwords :: Char -> [String] -> String
unwords _ [] = ""
unwords p (w : ws) = w ++ go ws
  where
    go [] = ""
    go (v : vs) = p : (v ++ go vs)

parseTimeStr :: String -> Maybe DiffTime
parseTimeStr str = do
  -- Parse into TimeOfDay
  t <- parseTimeM True defaultTimeLocale "%T" str :: Maybe TimeOfDay
  -- Convert to seconds since midnight
  return $ timeOfDayToTime t

-- Add two time strings
addTimeStrings :: String -> String -> Maybe DiffTime
addTimeStrings t1 t2 = do
  d1 <- parseTimeStr t1
  d2 <- parseTimeStr t2
  return $ d1 + d2

-- Convert DiffTime back to "HH:MM:SS"
diffTimeToStr :: DiffTime -> String
diffTimeToStr dt = formatTime defaultTimeLocale "%T" (timeToTimeOfDay dt)

addTimes :: String -> String -> String
addTimes t1 t2 = diffTimeToStr $ fromMaybe 0 $ addTimeStrings t1 t2