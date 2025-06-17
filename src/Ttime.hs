module Ttime
  ( getDate,
    date2string,
  )
  where


import Data.Time.Clock
import Data.Time.Calendar
import Data.Functor
import Text.Printf (printf)

import Ytils

getDate :: IO (Integer, Int, Int) -- :: (year, month, day)
getDate = getCurrentTime <&> (toGregorian . utctDay)


date2string :: (Integer, Int, Int) -> String
date2string (y, m, d) = Ytils.unwords '-' [show y, printf "%02d" m, printf "%02d" d]
