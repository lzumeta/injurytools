# Get the year

Get the year given the season.

## Usage

``` r
season2year(season)
```

## Arguments

- season:

  Character/factor specifying the season. It should follow the pattern
  "xxxx/yyyy", e.g. "2005/2006".

## Value

Given the season, it returns the year (in `numeric`) in which the season
started.

## Examples

``` r
season <- "2022/2023"
season2year(season)
#> [1] 2022
```
