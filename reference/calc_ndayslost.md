# Calculate number of days lost

Calculate the number of days lost due to a sports-related health problem
(e.g. injuries) in a cohort during a period.

## Usage

``` r
calc_ndayslost(injd, by = NULL, overall = TRUE)
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

The number of days lost. Either a numeric value (if overall `TRUE`) or a
data frame indicating the number of cases per athlete.

## Examples

``` r
calc_ndayslost(injd)
#> ndayslost 
#>      2049 
calc_ndayslost(injd, overall = FALSE)
#> # A tibble: 28 × 2
#>    person_id               ndayslost
#>    <fct>                       <dbl>
#>  1 adam-lallana                  302
#>  2 alberto-moreno                 50
#>  3 alex-oxlade-chamberlain       316
#>  4 alisson                         0
#>  5 andrew-robertson               22
#>  6 daniel-sturridge              122
#>  7 danny-ings                      0
#>  8 dejan-lovren                  160
#>  9 divock-origi                    5
#> 10 dominic-solanke                 0
#> # ℹ 18 more rows
calc_ndayslost(injd, by = "injury_type")
#> # A tibble: 5 × 2
#>   injury_type ndayslost
#>   <fct>           <dbl>
#> 1 Bone              173
#> 2 Concussion        213
#> 3 Ligament          596
#> 4 Muscle            735
#> 5 Unknown           332
```
