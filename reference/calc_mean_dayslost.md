# Calculate the mean days lost

Calculate the mean of the days lost due to a sports-related health
problem (e.g. disease, injury) in a cohort.

## Usage

``` r
calc_mean_dayslost(injd, by = NULL, overall = TRUE)
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

The mean of the days lost. Either a numeric value (if overall `TRUE`) or
a data frame indicating the mean days lost per athlete.

## Examples

``` r
calc_mean_dayslost(injd)
#> mean_dayslost 
#>       24.9878 
calc_mean_dayslost(injd, overall = FALSE)
#> # A tibble: 23 × 2
#>    person_id               mean_dayslost
#>    <fct>                           <dbl>
#>  1 adam-lallana                    50.3 
#>  2 alberto-moreno                  50   
#>  3 alex-oxlade-chamberlain        316   
#>  4 andrew-robertson                 4.4 
#>  5 daniel-sturridge                40.7 
#>  6 dejan-lovren                    26.7 
#>  7 divock-origi                     5   
#>  8 emre-can                        68   
#>  9 fabinho                          7.33
#> 10 georginio-wijnaldum              8.5 
#> # ℹ 13 more rows
calc_mean_dayslost(injd, by = "injury_type")
#> # A tibble: 5 × 2
#>   injury_type mean_dayslost
#>   <fct>               <dbl>
#> 1 Bone                 15.7
#> 2 Concussion           13.3
#> 3 Ligament             66.2
#> 4 Muscle               29.4
#> 5 Unknown              15.8
```
