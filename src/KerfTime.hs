{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module KerfTime  where
import           Data.Time









newtype UTCKerfTime = UTCKerfTime UTCTime
  deriving (Eq,Ord,Show)

class (Eq a, Ord a) => ToUTCKerfTime a where
  kerf :: a -> UTCKerfTime

class FromUTCKerfTime a where
  unkerf ::  UTCKerfTime -> a

class (ToUTCKerfTime a, FromUTCKerfTime a) => KerfedTime a where
  addYear :: a -> KerfYear -> a
  addMonth :: a -> KerfMonth -> a
  addWeek :: a -> KerfWeek -> a
  addDay :: a -> KerfDay -> a
  addHour :: a -> KerfHour -> a
  addMinute :: a -> KerfMin -> a
  addSecond :: a -> KerfSec -> a
  addPicosecond :: a -> KerfPico -> a






class KerfAdd a where
  add :: (KerfedTime t) => t -> a -> t

instance KerfAdd KerfYear where
  add =  addYear

instance KerfAdd KerfMonth where
  add =  addMonth

instance KerfAdd KerfWeek where
  add =  addWeek

instance KerfAdd KerfDay where
  add =  addDay

instance KerfAdd KerfHour where
  add =  addHour

instance KerfAdd KerfMin where
  add =  addMinute

instance KerfAdd KerfSec where
  add =  addSecond

instance KerfAdd KerfPico where
  add =  addPicosecond



newtype KerfYear   = KerfYear  Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfMonth  = KerfMonth Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfWeek = KerfWeek Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfDay    = KerfDay   Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfMin    = KerfMin   Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfHour   = KerfHour  Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfSec    = KerfSec   Integer
  deriving (Num,Eq,Ord,Show)
newtype KerfPico   = KerfPico  Integer -- Kerf uses nano but whatev
  deriving (Num, Eq, Ord, Show)


-- | NominalKerf time will obey the laws of KerfedTime by default
instance ToUTCKerfTime UTCKerfTime where
  kerf = id

instance FromUTCKerfTime UTCKerfTime where
  unkerf  = id


instance KerfedTime UTCKerfTime where
  addYear (UTCKerfTime k) y = UTCKerfTime $ addYear k y
  addMonth (UTCKerfTime k) m = UTCKerfTime $ addMonth k m
  addWeek (UTCKerfTime k) w = UTCKerfTime $ addWeek k w
  addDay (UTCKerfTime k) d = UTCKerfTime $ addDay k d
  addHour (UTCKerfTime k) h = UTCKerfTime $ addHour k h
  addMinute (UTCKerfTime k) i = UTCKerfTime $ addMinute k i
  addSecond (UTCKerfTime k) s = UTCKerfTime $ addSecond k s
  addPicosecond (UTCKerfTime k) p = UTCKerfTime $ addPicosecond k p


-- | UTCTime is the underlying and most important KerfTime thing
instance ToUTCKerfTime UTCTime where
  kerf = UTCKerfTime

instance FromUTCKerfTime UTCTime where
  unkerf (UTCKerfTime u) = u


instance KerfedTime UTCTime where
  addYear (UTCTime d t) (KerfYear y) = UTCTime (addGregorianYearsRollOver y d) t
  addMonth (UTCTime d t) (KerfMonth m) = UTCTime (addGregorianMonthsRollOver m d) t
  addWeek  (UTCTime d t) (KerfWeek w) = UTCTime (addDays (7*w) d) t
  addDay (UTCTime d t) (KerfDay ds) = UTCTime (addDays ds d) t
  addHour u (KerfHour h) = addUTCTime (fromIntegral $ h*3600) u
  addMinute u (KerfMin i) = addUTCTime (fromIntegral $ i*60) u
  addSecond u (KerfSec s) = addUTCTime (fromIntegral s) u
  addPicosecond u (KerfPico p) = addUTCTime (toNominal p) u
    where
      toNominal = fromRational . toRational .  picosecondsToDiffTime




-- | Usage: Add Intervals of different amounts
-- >>> date 2016 01 01 `add` (hour 3) `add` (week 16) `add` (month 3)
-- UTCKerfTime 2016-07-22 03:00:00 UTC

-- | Usage: Use negative signs to subtract
-- >>> date 2016 01 01 `add` hour (-3) `add` week (-16) `add` month (-3)
-- UTCKerfTime 2015-06-10 21:00:00 UTC


year :: Integer -> KerfYear
year = KerfYear

month :: Integer -> KerfMonth
month = KerfMonth

week :: Integer -> KerfWeek
week = KerfWeek

day :: Integer -> KerfDay
day = KerfDay

hour :: Integer -> KerfHour
hour = KerfHour

minute :: Integer -> KerfMin
minute = KerfMin

second :: Integer -> KerfSec
second = KerfSec

pico :: Integer -> KerfPico
pico = KerfPico



-- ==================================================
-- Make some KerfTime
-- ==================================================

-- | date 2016 01 15 -> UTCTime
date :: Integer -> Integer-> Integer-> UTCKerfTime
date y m d = UTCKerfTime $  UTCTime dayPart timePart
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

timePico :: Integer -> Integer -> Integer -> Integer -> DiffTime
timePico h m s p = picoTime +
                   (time h m s)
 where
   picoTime = picosecondsToDiffTime p

dateTime :: Integer -> Integer -> Integer ->
            Integer -> Integer -> Integer -> UTCKerfTime
dateTime y m d h i s = UTCKerfTime $ UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = time h i s

dateTimePico :: Integer -> Integer -> Integer
             -> Integer -> Integer  -> Integer -> Integer
             -> UTCKerfTime
dateTimePico y m d h i s p = UTCKerfTime $ UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = timePico h i s p



















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

getPicoseconds :: UTCTime -> Integer
getPicoseconds u@(UTCTime _ t) = round $ remainingPico *
                                  (fromRational (10^(12 :: Integer)))
  where
    remainingPico = t - (fromIntegral $ secondsInHours - secondsInMinutes - seconds')
    secondsInHours = 3600 * (getHour u)
    secondsInMinutes = 60 * (getMin u)
    seconds' =  getSeconds u
