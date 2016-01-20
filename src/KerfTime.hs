{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures      #-}
module KerfTime () where

import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Data.Time

import           Text.ParserCombinators.Parsec.Language
-- import           KerfTime.Internal
{--

Fundamental Type:
  STAMP
          2015.03.31 or 01:23:45.877 or 2015.03.31T01:23:45.123


Time Math:

  2015.04.01 + 1y + 1m + 1d
    2016.05.02

And 2015.04.01 + 1h2i3s gives 2015.04.01T01:02:03.000.

now_date() + 1d
  2015.04.02

now_time()
  21:36:00.762

minus(now_time(), 25 * 1h)
  20:36:02.005

Possible 'Units'
y: year
m: month
d: day
h: hour
i: minute
s: second
--}


-- data KerfTime = KerfTimeDate UTCTime | KerfTimeTime DiffTime

data Kerf = KerfYear   !Integer
           | KerfMonth !Integer
           | KerfDay   !Integer
           | KerfMin   !Integer
           | KerfHour  !Integer
           | KerfSec   !Integer


-- | HasKerfTime instances have to take care of marshalling on their inputs
class HasKerfTime a where
  year   :: a -> Kerf
  month  :: a -> Kerf
  day    :: a -> Kerf
  hour   :: a -> Kerf
  min    :: a -> Kerf
  second :: a -> Kerf
  pico   :: a -> Kerf
  fromKerf :: Kerf -> a
  toKerf :: a -> Kerf





-- --------------------------------------------------
-- UTC Time manipulation
-- ==================================================

-- | date 2016 01 15 -> UTCTime
date :: Integer -> Integer-> Integer-> UTCTime
date y m d = UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = 0


-- | Time only, you can't just add a diff time to a date so we get a diff time back
--
time :: Integer -> Integer -> Integer -> DiffTime
time h m s = secondsToDiffTime ( convertedHours  +
                                 convertedMinutes +
                                 convertedSeconds)
  where
    convertedHours = h * 3600
    convertedMinutes = m * 60
    convertedSeconds = s


dateTime :: Integer -> Integer -> Integer ->
            Integer -> Integer -> Integer -> UTCTime
dateTime y m d h i s = UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = time h i s




















--------------------------------------------------
-- Retrieval
--------------------------------------------------
-- | getYear someUtcTime -> 2016y
getYear :: UTCTime -> Integer
getYear incomingTime = case incomingTime of
   (UTCTime d _) -> let (year',_ ,_ ) = toGregorian d
                     in year'


-- | getMonth someUtcTime -> 10m
getMonth :: UTCTime -> Integer
getMonth incomingTime = case incomingTime of
   (UTCTime d _) -> let (_ , month' ,_ ) = toGregorian d
                    in  fromIntegral month'


-- | getDay someUtcTime -> 31d
getDay :: UTCTime -> Integer
getDay incomingTime = case incomingTime of
   (UTCTime d _) -> let (_ , _ ,day' ) = toGregorian d
                    in  fromIntegral day'


-- | Get all date parts together
getDateParts :: UTCTime -> (Integer,Integer,Integer)
getDateParts (UTCTime d _)  = (year',fromIntegral month',fromIntegral day')
  where
    (year',month',day') = toGregorian d

-- | getHour someUtcTime -> 1h
getHour :: UTCTime -> Integer
getHour (UTCTime _ t) = floor (t / 3600)

-- | getMin someUtcTime -> 37i
getMin :: UTCTime -> Integer
getMin u@(UTCTime _ t) = div remainingSeconds 60
  where
    timeInSeconds = floor t
    remainingSeconds = timeInSeconds - secondsInHours
    secondsInHours = 3600 * (getHour u)


-- | getSeconds someUtcTime -> 1s
getSeconds :: UTCTime -> Integer
getSeconds u@(UTCTime _ t) = remainingSeconds
  where
    timeInSeconds = floor t
    remainingSeconds = timeInSeconds - secondsInHours - secondsInMinutes
    secondsInHours = 3600 * (getHour u)
    secondsInMinutes = 60 * (getMin u)

