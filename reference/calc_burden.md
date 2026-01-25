# Calculate case burden rate

Calculate the case burden rate of a sports-related health problem (e.g.
disease, injury) in a cohort.

## Usage

``` r
calc_burden(
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

The case burden rate. Either a numeric value (if overall `TRUE`) or a
data frame indicating the case burden rate per athlete.

## References

Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the
burden of injuries and illnesses, not just their incidence. *British
Journal of Sports Medicine*, 52(16), 1018–1021.
[doi:10.1136/bjsports-2017-098160](https://doi.org/10.1136/bjsports-2017-098160)

Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L., ...
& Andersen T. E. (2023). Football-specific extension of the IOC
consensus statement: methods for recording and reporting of
epidemiological data on injury and illness in sport 2020. *British
journal of sports medicine*.

## Examples

``` r
calc_burden(injd)
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 1 × 6
#>   totalexpo ndayslost burden burden_sd burden_lower burden_upper
#>       <dbl>     <dbl>  <dbl>     <dbl>        <dbl>        <dbl>
#> 1     74690      2049   247.      5.45         236.         258.
calc_burden(injd, overall = FALSE)
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 28 × 7
#>    person_id      totalexpo ndayslost burden burden_sd burden_lower burden_upper
#>    <fct>              <dbl>     <dbl>  <dbl>     <dbl>        <dbl>        <dbl>
#>  1 adam-lallana         700       302 3883.     223.         3445.        4321. 
#>  2 alberto-moreno      1264        50  356.      50.3         257.         455. 
#>  3 alex-oxlade-c…      1483       316 1918.     108.         1706.        2129. 
#>  4 alisson             3420         0    0        0             0            0  
#>  5 andrew-robert…      5162        22   38.4      8.18         22.3         54.4
#>  6 daniel-sturri…       927       122 1184.     107.          974.        1395. 
#>  7 danny-ings           265         0    0        0             0            0  
#>  8 dejan-lovren        3109       160  463.      36.6         391.         535. 
#>  9 divock-origi         366         5  123.      55.0          15.2        231. 
#> 10 dominic-solan…       581         0    0        0             0            0  
#> # ℹ 18 more rows
calc_burden(injd, by = "injury_type")
#> Warning: 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> 
#> # A tibble: 5 × 7
#>   injury_type totalexpo ndayslost burden burden_sd burden_lower burden_upper
#>   <chr>           <dbl>     <dbl>  <dbl>     <dbl>        <dbl>        <dbl>
#> 1 Bone            74690       173   20.8      1.58         17.7         24.0
#> 2 Concussion      74690       213   25.7      1.76         22.2         29.1
#> 3 Ligament        74690       596   71.8      2.94         66.1         77.6
#> 4 Muscle          74690       735   88.6      3.27         82.2         95.0
#> 5 Unknown         74690       332   40.0      2.20         35.7         44.3
```
