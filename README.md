# herf-time

Haskell's time library is nice but I wanted to be able to do some of the operations in
the [Kerf](https://github.com/kevinlawler/kerf) programming language.


By creating a set of type classes: **HerfedTime** , **ToUTCHerfTime**, **FromUTCHerfTime**
I think there is a nice mechanism to do just that.

any instance of these 3 classes should satisfy the law:

* **Path Independant** on all addtion 
* ``` addX a X  == unherf (addX (herf a) X )```
  * where: ``` addX := {addYear , addMonth, addWeek ...}```


## Usage

### Add Intervals
``` haskell
>>> date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3
UTCHerfTime 2016-07-22 03:00:00 UTC
```

### Subtract Intervals
``` haskell

>>> date 2016 01 01 `add` hour (-3) `add` week (-16) `add` month (-3)
UTCHerfTime 2015-06-10 21:00:00 UTC
```

### Represent Time in Multiple Ways
``` haskell

>>> dateTime 2016 01 01 01 23 01 `add` hour 3 `add` week 16 `add` month 3
UTCHerfTime 2016-07-22 04:23:01 UTC
>>> dateTimePico 2016 01 01 01 23 01 01 `add` hour 3 `add` week 16 `add` month 3
UTCHerfTime 2016-07-22 04:23:01.000000000001 UTC
```
### Get Times in any HerfedTime format  (UTC for example)
``` haskell
>>> unherf $ date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3   :: UTCTime
2016-07-22 03:00:00 UTC
```

### Some fancier examples (Using HerfTime.ZonedTime)

``` haskell
λ> zt <- getZonedTime
λ> herfShow zt
"2016-01-21T11:29:05:CST"
λ> reherfz zt :: HerfZonedTime "PST"
2016-01-21T09:29:05:PST
λ> reherfz zt :: HerfZonedTime "+0600"
2016-01-21T23:29:05:+0600
λ> reherfz zt :: HerfZonedTime "CST"
2016-01-21T11:29:05:CST
λ> (reherfz zt) `add` month 2 :: HerfZonedTime "CST"
2016-03-21T11:29:05:CST

-- What time will it be exactly 3 months from now in California?
λ> (reherfz zt) `add` month 2 :: HerfZonedTime "PST"
2016-03-21T09:29:05:PST
```


### Use HerfTime.ZonedTime to convert easily between times
(reherf $ ( dateTime 2016 01 01 01 01 01 :: HerfZonedTime "CST")) :: HerfZonedTime "PST"
2015-12-31T23:01:01:PST

```
cabal configure --enable-tests && cabal build && cabal test
```


