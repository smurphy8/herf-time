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


data KerfTime = KerfTimeDate UTCTime | KerfTimeTime NominalDiffTime

 

timeOnly :: Integer -> Int -> Int -> KerfTime
timeOnly h m s = _



-- | dateOnly 2016 01 15 -> KerfTime

dateOnly :: Integer -> Int -> Int -> KerfTime
dateOnly y m d = KerfTimeDate $ UTCTime dayPart timePart
  where
    dayPart = fromGregorian y m d
    timePart = 0


-- | year someKerfTime -> 2016
-- a 0 is returned for a timestamp that has no date
year :: KerfTime -> Integer
year kerfTime = case kerfTime of
   (KerfTimeDate (UTCTime d _)) -> let (year',_ ,_ ) = toGregorian d
                                   in year'
   (KerfTimeTime dt) -> 0



-- | month someKerfTime -> 10
-- a 0 is returned for a timestamp that has no date
month :: KerfTime -> Int
month kerfTime = case kerfTime of
      (KerfTimeDate (UTCTime d _)) -> let (_,month' ,_ ) = toGregorian d
                                      in month'
      (KerfTimeTime dt) -> 0

-- | day someKerfTime -> 31
-- a 0 is returned for a timestamp that has no date

day :: KerfTime -> Int
day kerfTime = case kerfTime of
       (KerfTimeDate (UTCTime d _)) -> let (_,_ ,day' ) = toGregorian d
                                       in day'
       (KerfTimeTime dt) -> 0
