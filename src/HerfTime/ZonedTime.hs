{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{- |
Module      : HerfTime.LocalTime
Description : LocalTime interpreter for HerfedTime Class
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


The basic idea is to use the regular local time implementation available in the time library
but with a few nice wrappers.

For HerfTime to work nicely the information about what makes a time different should reside in the type.

in the case of zoned time, this is the time zone:

HerfZonedTime knows about these time zones by name:

RFC 822 sec. 5: "UT", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT".



| -}


module HerfTime.ZonedTime ( HerfZonedTime
                          , toZonedTime
                          , fromZonedTime
                          , addTimeZone
                          , herfz
                          , reherfz
                          , module HerfTime
                          , module Data.Time) where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromMaybe)
import           Data.Proxy
import           Data.Time
import           GHC.TypeLits
import           HerfTime

-- | Zoned Time always has an extra parameter to convert into a fixed time

newtype HerfZonedTime (z::Symbol)  = HerfZonedTime {_unherfZonedTime :: ZonedTime}
  deriving (FormatTime,ParseTime)
instance (KnownSymbol z) => Show (HerfZonedTime z) where
  show = herfShow

-- | Add Time Zone is different than converting
-- It takes a UTCTime and just slaps on the timezone in the ZonedTime field
-- This is exactly what you want when you are first creating a time
-- and almost never what you want after!


addTimeZone :: forall z. (KnownSymbol z) =>  UTCTime -> HerfZonedTime z
addTimeZone (UTCTime day' diffTime') = HerfZonedTime zonedTime
  where
    zonedTime = ZonedTime localTime timeZone
    localTime = LocalTime day' (timeToTimeOfDay diffTime')
    pz = Proxy  :: Proxy z
    v = symbolVal pz
    timeZone :: TimeZone
    timeZone = case (parseTimeM True defaultTimeLocale "%Z" v ) of
                  Just t -> t
                  Nothing -> fromMaybe (error $ "time zone broken " ++ v) (parseTimeM True defaultTimeLocale "%z" v)



-- | Helper function to convert UTCTime to HerfZoned time
toZonedTime :: forall z . (KnownSymbol z) =>  UTCTime -> HerfZonedTime z
toZonedTime time' = HerfZonedTime $ utcToZonedTime (fromMaybe utc . tz $ directSymbolVal) time'
  where
    pz = Proxy  :: Proxy z
    directSymbolVal = symbolVal pz
    tz :: String -> Maybe TimeZone
    tz v = (parseTimeM True defaultTimeLocale "%Z" v ) <|>
           (parseTimeM True defaultTimeLocale "%z" v)


-- | Getting back from HerfZonedTime to UTCTime
fromZonedTime :: forall z . (KnownSymbol z) => HerfZonedTime z -> UTCTime
fromZonedTime (HerfZonedTime time') = zonedTimeToUTC time'



instance (KnownSymbol z) =>  ToUTCHerfTime (HerfZonedTime z) where
  herf = herf . fromZonedTime

instance (KnownSymbol z) => FromUTCHerfTime (HerfZonedTime z) where
  unherf = toZonedTime . unherf

-- |
-- >>> :set -XDataKinds
-- >>> (reherf $ ( dateTime 2016 01 01 01 01 01 :: HerfZonedTime "CST")) :: HerfZonedTime "PST"
-- 2015-12-31T23:01:01:PST

instance (KnownSymbol z) => HerfedTime (HerfZonedTime z) where
  addYear a y  = unherf $ herf a `add` y
  addMonth a m  = unherf $ herf a `add` m
  addWeek a w  = unherf $ herf a `add` w
  addDay a d  = unherf $ herf a `add` d
  addHour a h  = unherf $ herf a `add` h
  addMinute a i  = unherf $ herf a `add` i
  addSecond a s  = unherf $ herf a `add` s
  addPicosecond a p  = unherf $ herf a `add` p
  date y m d = toZonedTime $ date y m d
  dateTime y m d h i s = addTimeZone $ dateTime y m d h i s
  dateTimePico y m d h i s p = addTimeZone $ dateTimePico y m d h i s p


-- | Don't want an orphan on ZonedTime so I made herfz
herfz :: ZonedTime -> UTCHerfTime
herfz = herf . zonedTimeToUTC


-- | like 'reherf' but for zoned time (which doesn't have a direct HerfedTime instance)
reherfz :: FromUTCHerfTime b => ZonedTime -> b
reherfz = unherf . herfz


