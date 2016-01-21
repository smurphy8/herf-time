{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
The Herf Time package is loosely based on the Kerf programming language's approach to time.
<https://github.com/kevinlawler/kerf>

Where it made sense to do things differently it does.

The fundamental engine is the typeclass "HerfTime" -}


module HerfTime  where

import           Data.Time




-- | Add Intervals of different amounts

-- | >>> date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3 :: UTCHerfTime
-- UTCHerfTime 2016-07-22 03:00:00 UTC

-- | Represent Time in a few different ways

-- | >>> dateTime 2016 01 01 01 23 01 `add` (hour 3) `add` (week 16) `add` (month 3) :: UTCHerfTime
-- UTCHerfTime 2016-07-22 04:23:01 UTC
-- >>> dateTimePico 2016 01 01 01 23 01 01 `add` (hour 3) `add` (week 16) `add` (month 3) :: UTCHerfTime
-- UTCHerfTime 2016-07-22 04:23:01.000000000001 UTC


-- | Use negative signs to subtract

-- | >>> date 2016 01 01 `add` hour (-3) `add` week (-16) `add` month (-3) :: UTCHerfTime
-- UTCHerfTime 2015-06-10 21:00:00 UTC


year :: Integer -> HerfYear
year = HerfYear

month :: Integer -> HerfMonth
month = HerfMonth

week :: Integer -> HerfWeek
week = HerfWeek

day :: Integer -> HerfDay
day = HerfDay

hour :: Integer -> HerfHour
hour = HerfHour

minute :: Integer -> HerfMin
minute = HerfMin

second :: Integer -> HerfSec
second = HerfSec

pico :: Integer -> HerfPico
pico = HerfPico


herfShow :: FormatTime t => t -> String
herfShow = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S:%Z") )  -- i.e. YYYY-MM-DDTHH:MM:SS

-- | >>> date 1 1 1 :: UTCHerfTime
-- UTCHerfTime 0001-01-01 00:00:00 UTC

dateHerf :: Integer -> Integer-> Integer-> UTCHerfTime
dateHerf y m d = UTCHerfTime $  UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = 0


-- | Time only, you can't just add a diff time to a date so we get a diff time back
-- >>> time 1 1 1
-- 3661s
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

dateTimeHerf :: Integer -> Integer -> Integer ->
                Integer -> Integer -> Integer -> UTCHerfTime
dateTimeHerf y m d h i s = UTCHerfTime $ UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = time h i s

dateTimePicoHerf :: Integer -> Integer -> Integer
                 -> Integer -> Integer  -> Integer -> Integer
                 -> UTCHerfTime
dateTimePicoHerf y m d h i s p = UTCHerfTime $ UTCTime dayPart timePart
  where
    dayPart = fromGregorian y (fromInteger m) (fromInteger d)
    timePart = timePico h i s p


-- --------------------------------------------------

-- | Below are the classes that make up the core of the
-- HerfTime Library.
-- Starting with the type 'UTCHerfTime'  which is the
-- encoding that  most other time stamps pass through

newtype UTCHerfTime = UTCHerfTime UTCTime
  deriving (Eq,Ord,Show,FormatTime)


-- | The 'ToUTCHerfTime' is necessary to have an interface 'lifted'
-- so that all the functions can be abstracted over it

class  ToUTCHerfTime a where
  herf :: a -> UTCHerfTime


class FromUTCHerfTime a where
  unherf ::  UTCHerfTime -> a


-- | 'reherf' is not part of a typeclass, just more sugar to make dealing with time conversion
-- If both classes are defined on the same type, the classes should round trip.
-- e.g.
-- (date y m d ):: UTCTime in d == (reherf d))


reherf :: (ToUTCHerfTime a, ToUTCHerfTime b, FromUTCHerfTime a, FromUTCHerfTime b ) =>
          (a -> b)
reherf = unherf.herf


-- | This defines the time language of herf
-- the important rule here is path independence
-- (unherf $ (herf a) `add` i )  == (a `add` i)
--
-- This ensures that regardless of how you get to a time
-- the result will be the same
--
-- Something to notice is that rule still allows for lossy
-- Time Stamps.  The loss just has to be captured uniformally
-- in the transforms and the interval arithmetic

class (ToUTCHerfTime a, FromUTCHerfTime a) => HerfedTime a where
  addYear :: a -> HerfYear -> a
  addMonth :: a -> HerfMonth -> a
  addWeek :: a -> HerfWeek -> a
  addDay :: a -> HerfDay -> a
  addHour :: a -> HerfHour -> a
  addMinute :: a -> HerfMin -> a
  addSecond :: a -> HerfSec -> a
  addPicosecond :: a -> HerfPico -> a
  date :: HerfYear -> HerfMonth -> HerfDay -> a
  dateTime :: HerfYear -> HerfMonth -> HerfDay -> HerfHour -> HerfMin -> HerfSec ->  a
  dateTimePico :: HerfYear -> HerfMonth -> HerfDay -> HerfHour -> HerfMin -> HerfSec -> HerfPico ->  a



class HerfAdd a where
  add :: (HerfedTime t) => t -> a -> t

instance HerfAdd HerfYear where
  add =  addYear

instance HerfAdd HerfMonth where
  add =  addMonth

instance HerfAdd HerfWeek where
  add =  addWeek

instance HerfAdd HerfDay where
  add =  addDay

instance HerfAdd HerfHour where
  add =  addHour

instance HerfAdd HerfMin where
  add =  addMinute

instance HerfAdd HerfSec where
  add =  addSecond

instance HerfAdd HerfPico where
  add =  addPicosecond


newtype HerfYear   = HerfYear  Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfMonth  = HerfMonth Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfWeek = HerfWeek Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfDay    = HerfDay   Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfMin    = HerfMin   Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfHour   = HerfHour  Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfSec    = HerfSec   Integer
  deriving (Num,Eq,Ord,Show)
newtype HerfPico   = HerfPico  Integer -- Herf uses nano but whatev
  deriving (Num, Eq, Ord, Show)


-- | NominalHerf time will obey the laws of HerfedTime by default
instance ToUTCHerfTime UTCHerfTime where
  herf = id

instance FromUTCHerfTime UTCHerfTime where
  unherf  = id

instance HerfedTime UTCHerfTime where
  addYear (UTCHerfTime k) y = UTCHerfTime $ addYear k y
  addMonth (UTCHerfTime k) m = UTCHerfTime $ addMonth k m
  addWeek (UTCHerfTime k) w = UTCHerfTime $ addWeek k w
  addDay (UTCHerfTime k) d = UTCHerfTime $ addDay k d
  addHour (UTCHerfTime k) h = UTCHerfTime $ addHour k h
  addMinute (UTCHerfTime k) i = UTCHerfTime $ addMinute k i
  addSecond (UTCHerfTime k) s = UTCHerfTime $ addSecond k s
  addPicosecond (UTCHerfTime k) p = UTCHerfTime $ addPicosecond k p
  date (HerfYear y) (HerfMonth m) (HerfDay d) = dateHerf y m d
  dateTime (HerfYear y) (HerfMonth m) (HerfDay d) (HerfHour h) (HerfMin i) (HerfSec s ) = dateTimeHerf y m d h i s
  dateTimePico (HerfYear y) (HerfMonth m) (HerfDay d) (HerfHour h) (HerfMin i) (HerfSec s ) (HerfPico p ) = dateTimePicoHerf y m d h i s p
-- | UTCTime is the underlying and most important HerfTime thing
instance ToUTCHerfTime UTCTime where
  herf = UTCHerfTime

instance FromUTCHerfTime UTCTime where
  unherf (UTCHerfTime u) = u

-- | Get Times in any viable format (UTC for example)
-- >>> unherf $ date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3   :: UTCTime
-- 2016-07-22 03:00:00 UTC


instance HerfedTime UTCTime where
  addYear (UTCTime d t) (HerfYear y) = UTCTime (addGregorianYearsRollOver y d) t
  addMonth (UTCTime d t) (HerfMonth m) = UTCTime (addGregorianMonthsRollOver m d) t
  addWeek  (UTCTime d t) (HerfWeek w) = UTCTime (addDays (7*w) d) t
  addDay (UTCTime d t) (HerfDay ds) = UTCTime (addDays ds d) t
  addHour u (HerfHour h) = addUTCTime (fromIntegral $ h*3600) u
  addMinute u (HerfMin i) = addUTCTime (fromIntegral $ i*60) u
  addSecond u (HerfSec s) = addUTCTime (fromIntegral s) u
  addPicosecond u (HerfPico p) = addUTCTime (toNominal p) u
    where
      toNominal = fromRational . toRational .  picosecondsToDiffTime

  date (HerfYear y) (HerfMonth m) (HerfDay d) = unherf $ dateHerf y m d
  dateTime (HerfYear y) (HerfMonth m) (HerfDay d) (HerfHour h) (HerfMin i) (HerfSec s ) = unherf $ dateTimeHerf y m d h i s
  dateTimePico (HerfYear y) (HerfMonth m) (HerfDay d) (HerfHour h) (HerfMin i) (HerfSec s ) (HerfPico p ) = unherf $ dateTimePicoHerf y m d h i s p


















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
