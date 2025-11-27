# Get the season

Get the season given the date.

## Usage

``` r
date2season(date)
```

## Arguments

- date:

  A vector of class [Date](https://rdrr.io/r/base/Dates.html) or
  [integer](https://rdrr.io/r/base/integer.html)/[numeric](https://rdrr.io/r/base/numeric.html).
  If it is `integer`/`numeric`, it should refer to the year in which the
  season started (e.g. `date` = 2015 to refer to the 2015/2016 season)

## Value

Character specifying the respective competition season given the date.
The season (output) follows this pattern: "2005/2006".

## Examples

``` r
date <- Sys.Date()
date2season(date)
#> [1] "2025/2026"
```
