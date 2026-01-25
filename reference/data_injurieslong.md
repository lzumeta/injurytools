# Transform injury data into a long format

Transform injury data into a long format

## Usage

``` r
data_injurieslong(data_injuries)
```

## Arguments

- data_injuries:

  Injury data frame with standardized column names, in the same fashion
  that
  [`prepare_inj()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  returns.

## Value

The `data_injuries` data frame in long format in which each row
corresponds to person-event.
