# Calculate the median days lost

Calculate the median of the days lost due to a sports-related health
problem (e.g. disease, injury).

## Usage

``` r
calc_median_dayslost(injd, by = NULL, overall = TRUE)
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

The median of the days lost. Either a numeric value (if overall `TRUE`)
or a data frame indicating the median days lost per athlete.

## Examples

``` r
calc_median_dayslost(injd)
#> median_dayslost 
#>              12 
calc_median_dayslost(injd, overall = FALSE)
#> # A tibble: 23 × 2
#>    person_id               median_dayslost
#>    <fct>                             <dbl>
#>  1 adam-lallana                         44
#>  2 alberto-moreno                       50
#>  3 alex-oxlade-chamberlain             316
#>  4 andrew-robertson                      3
#>  5 daniel-sturridge                     51
#>  6 dejan-lovren                         17
#>  7 divock-origi                          5
#>  8 emre-can                             68
#>  9 fabinho                               9
#> 10 georginio-wijnaldum                   7
#> # ℹ 13 more rows
calc_median_dayslost(injd, by = "injury_type")
#> # A tibble: 5 × 2
#>   injury_type median_dayslost
#>   <fct>                 <dbl>
#> 1 Bone                    9  
#> 2 Concussion             10.5
#> 3 Ligament               28  
#> 4 Muscle                 21  
#> 5 Unknown                 7  
```
