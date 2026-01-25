# Transform calc_summary() output according to the unit of exposure

Transform calc_summary() output according to the unit of exposure

## Usage

``` r
summary_unit(unit, summary_data, quiet)
```

## Arguments

- unit:

  Character that indicates the unit of exposure of the sports-related
  health problem data.

- summary_data:

  `summary_data` data frame to be transformed.

- quiet:

  Logical, whether or not to silence the warning messages (defaults to
  `FALSE`).

## Value

The same calc_summary() data frame with 'incidence' and 'cburden' values
transformed according to `unit`, named `summary_data`.
