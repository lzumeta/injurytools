# Calculate number of cases

Calculate the number of sports-related cases (e.g. injuries) that
occurred in a cohort during a period.

## Usage

``` r
calc_ncases(injd, by = NULL, overall = TRUE)
```

## Arguments

- injd:

  `injd` **S3** object (see
  [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)).

- by:

  Character specifying the name of the column according to which compute
  summary statistics. It should refer to a (categorical) variable that
  describes a grouping factor (e.g. "type of case or injury", "injury
  location", "sports club"). Optional, defaults to `NULL`.

- overall:

  Logical, whether to calculate overall (for all the cohort) or
  athlete-wise summary statistic (i.e. number of cases per cohort of per
  athlete). Defaults to `TRUE`.

## Value

The number of cases. Either a numeric value (if overall `TRUE`) or a
data frame indicating the number of cases per athlete.

## Examples

``` r
calc_ncases(injd)
#> ncases 
#>     82 
calc_ncases(injd, overall = FALSE)
#> # A tibble: 28 × 2
#>    person_id               ncases
#>    <fct>                    <dbl>
#>  1 adam-lallana                 6
#>  2 alberto-moreno               1
#>  3 alex-oxlade-chamberlain      1
#>  4 alisson                      0
#>  5 andrew-robertson             5
#>  6 daniel-sturridge             3
#>  7 danny-ings                   0
#>  8 dejan-lovren                 6
#>  9 divock-origi                 1
#> 10 dominic-solanke              0
#> # ℹ 18 more rows
calc_ncases(injd, by = "injury_type")
#> # A tibble: 5 × 2
#>   injury_type ncases
#>   <fct>        <dbl>
#> 1 Bone            11
#> 2 Concussion      16
#> 3 Ligament         9
#> 4 Muscle          25
#> 5 Unknown         21
```
