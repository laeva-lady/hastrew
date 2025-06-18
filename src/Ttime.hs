module Ttime
  ( getDate,
    date2string,
  )
  where


import Data.Time
import Text.Printf (printf)

import Ytils

getDate :: IO (Integer, Int, Int) -- (year, month, day)
getDate = do
  zonedTime <- getZonedTime
  let (year, month, day) = toGregorian $ localDay $ zonedTimeToLocalTime zonedTime
  return (year, month, day)

date2string :: (Integer, Int, Int) -> String
date2string (y, m, d) = Ytils.unwords '-' [show y, printf "%02d" m, printf "%02d" d]
