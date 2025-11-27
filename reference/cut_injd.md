# Cut the range of the follow-up

Given an `injd` object, cut the range of the time period such that the
limits of the observed dates, first and last observed dates, are `date0`
and `datef`, respectively. It is possible to specify just one date, i.e.
the two dates of the range do not necessarily have to be entered. See
Note section.

## Usage

``` r
cut_injd(injd, date0, datef)
```

## Arguments

- injd:

  Prepared data, an `injd` object.

- date0:

  Starting date of class [Date](https://rdrr.io/r/base/Dates.html) or
  [numeric](https://rdrr.io/r/base/numeric.html). If `numeric`, it
  should refer to a year (e.g. `date` = 2018). Optional.

- datef:

  Ending date. Same class as `date0`. Optional.

## Value

An `injd` object with a shorter follow-up period.

## Note

Be aware that by modifying the follow-up period of the cohort, the study
design is being altered. This function should not be used, unless there
is no strong argument supporting it. And in that case, it should be used
with caution.

## Examples

``` r
# Prepare data
# \donttest{
df_injuries <- prepare_inj(
  df_injuries0   = raw_df_injuries,
  person_id      = "player_name",
  date_injured   = "from",
  date_recovered = "until"
)

df_exposures <- prepare_exp(
  df_exposures0 = raw_df_exposures,
  person_id     = "player_name",
  date          = "year",
  time_expo     = "minutes_played"
)

injd <- prepare_all(
  data_exposures = df_exposures,
  data_injuries  = df_injuries,
  exp_unit       = "matches_minutes"
)
# }

# \donttest{
cut_injd(injd, date0 = 2018)
#> Warning: Injury/illness data has been cut to the given follow-up period (in exposure data)
#> # A tibble: 74 × 19
#>    person_id        t0         tf         date_injured date_recovered tstart    
#>    <fct>            <date>     <date>     <date>       <date>         <date>    
#>  1 adam-lallana     2018-07-01 2019-06-30 2018-09-04   2018-10-19     2018-07-01
#>  2 adam-lallana     2018-07-01 2019-06-30 2018-11-09   2018-12-04     2018-10-19
#>  3 adam-lallana     2018-07-01 2019-06-30 2019-01-06   2019-01-18     2018-12-04
#>  4 adam-lallana     2018-07-01 2019-06-30 2019-04-01   2019-05-31     2019-01-18
#>  5 adam-lallana     2018-07-01 2019-06-30 NA           NA             2019-05-31
#>  6 alisson          2018-07-01 2019-06-30 NA           NA             2018-07-01
#>  7 andrew-robertson 2018-07-01 2019-06-30 2018-12-03   2018-12-07     2018-07-01
#>  8 andrew-robertson 2018-07-01 2019-06-30 2019-02-25   2019-02-26     2018-12-07
#>  9 andrew-robertson 2018-07-01 2019-06-30 2019-05-08   2019-05-11     2019-02-26
#> 10 andrew-robertson 2018-07-01 2019-06-30 2019-06-12   2019-06-24     2019-05-11
#> # ℹ 64 more rows
#> # ℹ 13 more variables: tstop <date>, tstart_minPlay <dbl>, tstop_minPlay <dbl>,
#> #   status <dbl>, enum <dbl>, days_lost <dbl>, player_id <fct>, season <fct>,
#> #   games_lost <dbl>, injury <chr>, injury_acl <fct>, injury_type <fct>,
#> #   injury_severity <fct>
# }
```
