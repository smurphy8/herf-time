# herf-time

Haskell's time library is nice but I wanted to be able to do some of the operations in
the Herf programming language.


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

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```


