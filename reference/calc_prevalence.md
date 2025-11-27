# Calculate prevalence proportion

Calculate the prevalence proportion of injured athletes and the
proportion of non-injured (available) athletes in the cohort, on a
monthly or season basis. Further information on the type of injury may
be specified so that the injury-specific prevalences are reported
according to this variable.

## Usage

``` r
calc_prevalence(injd, time_period = c("monthly", "season"), by = NULL)
```

## Arguments

- injd:

  Prepared data. An `injd` object.

- time_period:

  Character. One of "monthly" or "season", specifying the periodicity
  according to which to calculate the proportions of available and
  injured athletes.

- by:

  Character specifying the name of the column on the basis of which to
  classify the injuries and calculate proportions of the injured
  athletes. Defaults to `NULL`.

## Value

A data frame containing one row for each combination of season, month
(optionally) and injury type (if `by` not specified, then this variable
has two categories: *Available* and *Injured*). Plus, three more
columns, specifying the proportion of athletes (`prop`) satisfying the
corresponding row's combination of values, i.e. prevalence, how many
athletes were injured at that moment with the type of injury of the
corresponding row (`n`), over how many athletes were at that time in the
cohort (`n_athlete`). See Note section.

## Note

If `by` is specified (and not `NULL`), it may happen that an athlete in
one month suffers two different types of injuries. For example, a muscle
and a ligament injury. In this case, this two injuries contribute to the
proportions of muscle and ligament injuries for that month, resulting in
an overall proportion that exceeds 100%. Besides, the athletes in
Available category are those that did not suffer any injury in that
moment (season-month), that is, they were healthy all the time that the
period lasted.

## References

Bahr R, Clarsen B, Derman W, et al. International Olympic Committee
consensus statement: methods for recording and reporting of
epidemiological data on injury and illness in sport 2020 (including
STROBE Extension for Sport Injury and Illness Surveillance
(STROBE-SIIS)) *British Journal of Sports Medicine* 2020; 54:372-389.

Nielsen RO, Debes-Kristensen K, Hulme A, et al. Are prevalence measures
better than incidence measures in sports injury research? *British
Journal of Sports Medicine* 2019; 54:396-397.

## Examples

``` r
# \donttest{
df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
                            date = "year", time_expo = "minutes_played")
df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
                            date_injured = "from", date_recovered = "until")
injd         <- prepare_all(data_exposures = df_exposures,
                            data_injuries  = df_injuries,
                            exp_unit = "matches_minutes")
# }

calc_prevalence(injd, time_period = "monthly", by = "injury_type")
#> # A tibble: 98 × 6
#>    season           month status         n n_athlete  prop
#>    <fct>            <fct> <fct>      <int>     <int> <dbl>
#>  1 season 2017/2018 Jul   Available     21        23  91.3
#>  2 season 2017/2018 Jul   Muscle         2        23   8.7
#>  3 season 2017/2018 Aug   Available     18        23  78.3
#>  4 season 2017/2018 Aug   Muscle         3        23  13  
#>  5 season 2017/2018 Aug   Unknown        2        23   8.7
#>  6 season 2017/2018 Sep   Available     22        23  95.7
#>  7 season 2017/2018 Sep   Muscle         1        23   4.3
#>  8 season 2017/2018 Oct   Available     19        23  82.6
#>  9 season 2017/2018 Oct   Concussion     1        23   4.3
#> 10 season 2017/2018 Oct   Muscle         2        23   8.7
#> # ℹ 88 more rows
calc_prevalence(injd, time_period = "monthly")
#> # A tibble: 48 × 6
#>    season           month status        n n_athlete  prop
#>    <fct>            <fct> <fct>     <int>     <int> <dbl>
#>  1 season 2017/2018 Jul   Available    21        23  91.3
#>  2 season 2017/2018 Jul   Injured       2        23   8.7
#>  3 season 2017/2018 Aug   Available    18        23  78.3
#>  4 season 2017/2018 Aug   Injured       5        23  21.7
#>  5 season 2017/2018 Sep   Available    22        23  95.7
#>  6 season 2017/2018 Sep   Injured       1        23   4.3
#>  7 season 2017/2018 Oct   Available    19        23  82.6
#>  8 season 2017/2018 Oct   Injured       4        23  17.4
#>  9 season 2017/2018 Nov   Available    18        23  78.3
#> 10 season 2017/2018 Nov   Injured       5        23  21.7
#> # ℹ 38 more rows
calc_prevalence(injd, time_period = "season", by = "injury_type")
#> # A tibble: 11 × 5
#>    season           status         n n_athlete  prop
#>    <fct>            <fct>      <int>     <int> <dbl>
#>  1 season 2017/2018 Available      7        23  30.4
#>  2 season 2017/2018 Concussion     3        23  13  
#>  3 season 2017/2018 Ligament       4        23  17.4
#>  4 season 2017/2018 Muscle        11        23  47.8
#>  5 season 2017/2018 Unknown        8        23  34.8
#>  6 season 2018/2019 Available      2        19  10.5
#>  7 season 2018/2019 Bone          11        19  57.9
#>  8 season 2018/2019 Concussion    13        19  68.4
#>  9 season 2018/2019 Ligament       6        19  31.6
#> 10 season 2018/2019 Muscle        15        19  78.9
#> 11 season 2018/2019 Unknown       13        19  68.4
calc_prevalence(injd, time_period = "season")
#> # A tibble: 4 × 5
#>   season           status        n n_athlete  prop
#>   <fct>            <fct>     <int>     <int> <dbl>
#> 1 season 2017/2018 Available     7        23  30.4
#> 2 season 2017/2018 Injured      16        23  69.6
#> 3 season 2018/2019 Available     2        19  10.5
#> 4 season 2018/2019 Injured      17        19  89.5
```
