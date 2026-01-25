# Prepare data in a standardized format

These are the data preprocessing functions provided by the `injurytools`
package, which involve:

1.  setting **exposure** and **injury and illness data** in a
    standardized format and

2.  integrating both sources of data into an adequate data structure.

`prepare_inj()` and `prepare_exp()` set standardized names and proper
classes to the (key) columns in injury/illness and exposure data,
respectively. `prepare_all()` integrates both, standardized injury and
exposure data sets, and convert them into an `injd` **S3** object that
has an adequate structure for further statistical analyses. See the
[Prepare Sports Injury
Data](https://lzumeta.github.io/injurytools/articles/prepare-injury-data.html)
vignette for details.

## Usage

``` r
prepare_inj(
  df_injuries0,
  person_id = "person_id",
  date_injured = "date_injured",
  date_recovered = "date_recovered"
)

prepare_exp(
  df_exposures0,
  person_id = "person_id",
  date = "date",
  time_expo = "time_expo"
)

prepare_all(
  data_exposures,
  data_injuries,
  exp_unit = c("minutes", "hours", "days", "matches_num", "matches_minutes",
    "activity_days", "seasons")
)
```

## Arguments

- df_injuries0:

  A data frame containing injury or illness information, with columns
  referring to the athlete name/id, date of injury/illness and date of
  recovery (as minimal data).

- person_id:

  Character referring to the column name storing sportsperson (player,
  athlete) identification information.

- date_injured:

  Character referring to the column name where the information about the
  date of injury or illness is stored.

- date_recovered:

  Character referring to the column name where the information about the
  date of recovery is stored.

- df_exposures0:

  A data frame containing exposure information, with columns referring
  to the sportsperson's name/id, date of exposure and the total time of
  exposure of the corresponding data entry (as minimal data).

- date:

  Character referring to the column name where the exposure date
  information is stored. Besides, the column must be of class
  [Date](https://rdrr.io/r/base/Dates.html) or
  [integer](https://rdrr.io/r/base/integer.html)/[numeric](https://rdrr.io/r/base/numeric.html).
  If it is `integer`/`numeric`, it should refer to the year in which the
  season started (e.g. `date` = 2015 to refer to the 2015/2016 season).

- time_expo:

  Character referring to the column name where the information about the
  time of exposure in that corresponding date is stored.

- data_exposures:

  Exposure data frame with standardized column names, in the same
  fashion that `prepare_exp()` returns.

- data_injuries:

  Injury data frame with standardized column names, in the same fashion
  that `prepare_inj()` returns.

- exp_unit:

  Character defining the unit of exposure time ("minutes" the default).

## Value

`prepare_inj()` returns a data frame in which the **key columns** in
injury/illness data are standardized and have a proper format.

`prepare_exp()` returns a data frame in which the **key columns** in
exposure data are standardized and have a proper format.

`prepare_all()` returns the `injd` **S3** object that contains all the
necessary information and a proper data structure to perform further
statistical analyses (e.g. calculate injury summary statistics,
visualize injury data).

- If `exp_unit` is "minutes" (the default), the columns `tstart_min` and
  `tstop_min` are created which specify the time to event (injury)
  values, the starting and stopping time of the interval, respectively.
  That is the training time in minutes, that the sportsperson has been
  at risk, until an injury/illness (or censorship) has occurred. For
  other choices, `tstart_x` and `tstop_x` are also created according to
  the `exp_unit` indicated (`x`, one of: `min`, `h`, `match`, `minPlay`,
  `d`, `acd` or `s`). These columns will be useful for survival analysis
  routines. See Note section.

- It also creates `days_lost` column based on the difference between
  `date_recovered` and `date_injured` in days. And if it does exist (in
  the raw data) it overrides.

## Note

Depending on the unit of exposure, `tstart_x` and `tstop_x` columns
might have same values (e.g. if `exp_unit` = "matches_num" and the
player has not played any match between the corresponding period of
time). Please be aware of this before performing any survival analysis
related task.

## Examples

``` r
df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                           person_id      = "player_name",
                           date_injured   = "from",
                           date_recovered = "until")

df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                            person_id     = "player_name",
                            date          = "year",
                            time_expo     = "minutes_played")

# \donttest{
injd <- prepare_all(data_exposures = df_exposures,
                    data_injuries  = df_injuries,
                    exp_unit = "matches_minutes")
head(injd)
#> # A tibble: 6 × 19
#>   person_id    t0         tf         date_injured date_recovered tstart    
#>   <fct>        <date>     <date>     <date>       <date>         <date>    
#> 1 adam-lallana 2017-07-01 2019-06-30 2017-07-31   2017-11-25     2017-07-01
#> 2 adam-lallana 2017-07-01 2019-06-30 2018-03-31   2018-05-13     2017-11-25
#> 3 adam-lallana 2017-07-01 2019-06-30 2018-09-04   2018-10-19     2018-05-13
#> 4 adam-lallana 2017-07-01 2019-06-30 2018-11-09   2018-12-04     2018-10-19
#> 5 adam-lallana 2017-07-01 2019-06-30 2019-01-06   2019-01-18     2018-12-04
#> 6 adam-lallana 2017-07-01 2019-06-30 2019-04-01   2019-05-31     2019-01-18
#> # ℹ 13 more variables: tstop <date>, tstart_minPlay <dbl>, tstop_minPlay <dbl>,
#> #   status <dbl>, enum <dbl>, days_lost <dbl>, player_id <fct>, season <fct>,
#> #   games_lost <dbl>, injury <chr>, injury_acl <fct>, injury_type <fct>,
#> #   injury_severity <fct>
class(injd)
#> [1] "injd"       "tbl_df"     "tbl"        "data.frame"
str(injd, 1)
#> injd [108 × 19] (S3: injd/tbl_df/tbl/data.frame)
#>  - attr(*, "unit_exposure")= chr "matches_minutes"
# }
```
