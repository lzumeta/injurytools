# Proper Conversion of Date objects

Converts [Date](https://rdrr.io/r/base/Dates.html) objects into a common
format used for every Date object throughout the package.

## Usage

``` r
date_format(date)
```

## Arguments

- date:

  a [Date](https://rdrr.io/r/base/Dates.html) object.

## Value

A "%Y-%m-%d" formatted [Date](https://rdrr.io/r/base/Dates.html) object
with respect to a fixed locale and time zone, i.e. setting the LC_TIME
component to C and timezone to UTC (the safest choice, non-geographic
and Coordinated Universal Time).

## Details

To limit the scope of the changes to LC_TIME and the timezone, a
temporary locale modification is made using the `withr` package.
