{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{- |
Module      : HerfTime.LocalTime
Description : LocalTime interpreter for HerfedTime Class
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


The basic idea is to use the regular local time implementation available in the time library
but with a few nice wrappers.

For HerfTime to work nicely the information about what makes a time different should reside squarely in the type.



| -}


module HerfTime.ZonedTime ( HerfZonedTime
                          , toZonedTime
                          , fromZonedTime) where

import           Data.Maybe   (fromMaybe)
import           Data.Proxy
import           Data.Time
import           GHC.TypeLits
import           HerfTime


-- | Zoned Time always has an extra parameter to convert into a fixed time

newtype HerfZonedTime (z::Symbol)  = HerfZonedTime ZonedTime
  deriving (Show)


toZonedTime :: forall z . (KnownSymbol z) =>  UTCTime -> HerfZonedTime z
toZonedTime time' = HerfZonedTime $ utcToZonedTime tz time'
  where
    pz = Proxy  :: Proxy z
    v = symbolVal pz
    tz :: TimeZone
    tz = case (parseTimeM True defaultTimeLocale v "%Z") of
           Just t -> t
           Nothing -> fromMaybe utc (parseTimeM True defaultTimeLocale v "%z" )


fromZonedTime :: forall z . (KnownSymbol z) => HerfZonedTime z -> UTCTime
fromZonedTime (HerfZonedTime time') = zonedTimeToUTC time'



instance (KnownSymbol z) =>  ToUTCHerfTime (HerfZonedTime z) where
  herf = herf . fromZonedTime

instance (KnownSymbol z) => FromUTCHerfTime (HerfZonedTime z) where
  unherf = toZonedTime . unherf


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
  dateTime y m d h i s = toZonedTime $ dateTime y m d h i s
  dateTimePico y m d h i s p = toZonedTime $ dateTimePico y m d h i s p
