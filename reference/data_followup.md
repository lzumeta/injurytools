# Build follow-up data frame

Build follow-up data frame

## Usage

``` r
data_followup(data_exposures)
```

## Arguments

- data_exposures:

  Exposure data frame with standardized column names, in the same
  fashion that
  [`prepare_exp()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  returns.

## Value

A data frame in which each row corresponds to a sportsperson and his/her
first date (`t0`) and last date (`tf`) observed.
