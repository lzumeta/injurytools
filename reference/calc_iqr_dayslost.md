# Calculate the interquartile range days lost

Calculate the interquartile range of the days lost due to a
sports-related health problem (e.g. disease, injury) in a cohort.

## Usage

``` r
calc_iqr_dayslost(injd, by = NULL, overall = TRUE)
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

The interquartile range of the days lost. Either a numeric value (if
overall `TRUE`) or a data frame indicating the interquartile range of
the days lost per athlete.

## Examples

``` r
calc_iqr_dayslost(injd)
#> qt25_dayslost qt75_dayslost 
#>             6            25 
calc_iqr_dayslost(injd, overall = FALSE)
#> # A tibble: 23 × 3
#>    person_id               qt25_dayslost qt75_dayslost
#>    <fct>                           <dbl>         <dbl>
#>  1 adam-lallana                     29.5          56.2
#>  2 alberto-moreno                   50            50  
#>  3 alex-oxlade-chamberlain         316           316  
#>  4 andrew-robertson                  2             4  
#>  5 daniel-sturridge                 33.5          53  
#>  6 dejan-lovren                     10            32.2
#>  7 divock-origi                      5             5  
#>  8 emre-can                         68            68  
#>  9 fabinho                           5.5          10  
#> 10 georginio-wijnaldum               5.5          12.2
#> # ℹ 13 more rows
calc_iqr_dayslost(injd, by = "injury_type")
#> # A tibble: 5 × 3
#>   injury_type qt25_dayslost qt75_dayslost
#>   <fct>               <dbl>         <dbl>
#> 1 Bone                 4.5           16.5
#> 2 Concussion           5.75          14.5
#> 3 Ligament             7             54  
#> 4 Muscle              12             36  
#> 5 Unknown              4             18  
```
