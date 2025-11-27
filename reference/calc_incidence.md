# Calculate case incidence rate

Calculate the case incidence rate of a sports-related health problem
(e.g. disease, injury) in a cohort.

## Usage

``` r
calc_incidence(
  injd,
  by = NULL,
  overall = TRUE,
  method = c("poisson", "negbin", "zinfpois", "zinfnb"),
  se = TRUE,
  conf_level = 0.95,
  scale = TRUE,
  quiet = FALSE
)
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

- method:

  Method to estimate the incidence (burden) rate. One of "poisson",
  "negbin", "zinfpois" or "zinfnb"; that stand for Poisson method,
  negative binomial method, zero-inflated Poisson and zero-inflated
  negative binomial.

- se:

  Logical, whether to calculate the confidence interval related to the
  rate.

- conf_level:

  Confidence level (defaults to 0.95).

- scale:

  Logical, whether to transform the incidence and burden rates output
  according to the unit of exposure (defaults to `TRUE`).

- quiet:

  Logical, whether or not to silence the warning messages (defaults to
  `FALSE`).

## Value

The case incidence rate. Either a numeric value (if overall `TRUE`) or a
data frame indicating the case incidence rate per athlete.

## References

Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the
burden of injuries and illnesses, not just their incidence. *British
Journal of Sports Medicine*, 52(16), 1018–1021.
<https://doi.org/10.1136/bjsports-2017-098160>

Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L., ...
& Andersen T. E. (2023). Football-specific extension of the IOC
consensus statement: methods for recording and reporting of
epidemiological data on injury and illness in sport 2020. *British
journal of sports medicine*.

## Examples

``` r
calc_incidence(injd)
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 1 × 6
#>   totalexpo ncases incidence incidence_sd incidence_lower incidence_upper
#>       <dbl>  <dbl>     <dbl>        <dbl>           <dbl>           <dbl>
#> 1     74690     82      9.88         1.09            7.74            12.0
calc_incidence(injd, overall = FALSE)
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 28 × 7
#>    person_id             totalexpo ncases incidence incidence_sd incidence_lower
#>    <fct>                     <dbl>  <dbl>     <dbl>        <dbl>           <dbl>
#>  1 adam-lallana                700      6     77.1         31.5            15.4 
#>  2 alberto-moreno             1264      1      7.12         7.12           -6.84
#>  3 alex-oxlade-chamberl…      1483      1      6.07         6.07           -5.83
#>  4 alisson                    3420      0      0            0               0   
#>  5 andrew-robertson           5162      5      8.72         3.90            1.08
#>  6 daniel-sturridge            927      3     29.1         16.8            -3.83
#>  7 danny-ings                  265      0      0            0               0   
#>  8 dejan-lovren               3109      6     17.4          7.09            3.47
#>  9 divock-origi                366      1     24.6         24.6           -23.6 
#> 10 dominic-solanke             581      0      0            0               0   
#> # ℹ 18 more rows
#> # ℹ 1 more variable: incidence_upper <dbl>
calc_incidence(injd, by = "injury_type")
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 5 × 7
#>   injury_type totalexpo ncases incidence incidence_sd incidence_lower
#>   <chr>           <dbl>  <dbl>     <dbl>        <dbl>           <dbl>
#> 1 Bone            74690     11      1.33        0.400           0.542
#> 2 Concussion      74690     16      1.93        0.482           0.983
#> 3 Ligament        74690      9      1.08        0.361           0.376
#> 4 Muscle          74690     25      3.01        0.602           1.83 
#> 5 Unknown         74690     21      2.53        0.552           1.45 
#> # ℹ 1 more variable: incidence_upper <dbl>
calc_incidence(injd, by = "injury_type", scale = FALSE)
#> # A tibble: 5 × 7
#>   injury_type totalexpo ncases incidence incidence_sd incidence_lower
#>   <chr>           <dbl>  <dbl>     <dbl>        <dbl>           <dbl>
#> 1 Bone            74690     11  0.000147    0.0000444       0.0000602
#> 2 Concussion      74690     16  0.000214    0.0000536       0.000109 
#> 3 Ligament        74690      9  0.000120    0.0000402       0.0000418
#> 4 Muscle          74690     25  0.000335    0.0000669       0.000204 
#> 5 Unknown         74690     21  0.000281    0.0000614       0.000161 
#> # ℹ 1 more variable: incidence_upper <dbl>
```
