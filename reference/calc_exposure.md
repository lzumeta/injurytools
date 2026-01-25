# Calculate the exposure time

Calculate the time of exposure that each athlete, or the entire cohort
of athletes, has been at risk for a sport-related health problem.

## Usage

``` r
calc_exposure(injd, by = NULL, overall = TRUE, quiet = FALSE)
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

- quiet:

  Logical, whether or not to silence the warning messages (defaults to
  `FALSE`).

## Value

The total exposure time. Either a numeric value (if overall `TRUE`) or a
data frame indicating the total exposure time for each athlete.

## Examples

``` r
calc_exposure(injd)
#> totalexpo 
#>     74690 
calc_exposure(injd, overall = FALSE)
#> # A tibble: 28 × 2
#>    person_id               totalexpo
#>    <fct>                       <dbl>
#>  1 adam-lallana                  700
#>  2 alberto-moreno               1264
#>  3 alex-oxlade-chamberlain      1483
#>  4 alisson                      3420
#>  5 andrew-robertson             5162
#>  6 daniel-sturridge              927
#>  7 danny-ings                    265
#>  8 dejan-lovren                 3109
#>  9 divock-origi                  366
#> 10 dominic-solanke               581
#> # ℹ 18 more rows
calc_exposure(injd, by = "injury_type")
#> # A tibble: 5 × 2
#>   injury_type totalexpo
#>   <chr>           <dbl>
#> 1 Bone            74690
#> 2 Concussion      74690
#> 3 Ligament        74690
#> 4 Muscle          74690
#> 5 Unknown         74690
```
