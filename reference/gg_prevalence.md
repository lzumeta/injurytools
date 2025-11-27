# Plot bar plots representing players' prevalence

Plot the proportions of available and injured players in the cohort, on
a monthly or season basis, by a bar plot. Further information on the
type of injury may be specified so that the injured players proportions
are disaggregated and reported according to this variable.

## Usage

``` r
gg_prevalence(
  injd,
  time_period = c("monthly", "season"),
  by = NULL,
  line_mean = FALSE,
  title = NULL
)
```

## Arguments

- injd:

  Prepared data, an `injd` object.

- time_period:

  Character. One of "monthly" or "season", specifying the periodicity
  according to which to calculate the proportions of available and
  injured athletes.

- by:

  Character specifying the name of the column on the basis of which to
  classify the injuries and calculate proportions of the injured
  athletes. Defaults to `NULL`.

- line_mean:

  Logical (defaults to FALSE) whether to add a horizontal line
  indicating the mean prevalence over the period.

- title:

  Text for the main title.

## Value

A ggplot object (to which optionally more layers can be added).

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

# \donttest{
library(ggplot2)
our_palette <- c("red3", rev(RColorBrewer::brewer.pal(5, "Reds")), "seagreen3")
gg_prevalence(injd, time_period = "monthly",
              title = "Monthly prevalence of sports injuries") +
  scale_fill_manual(values = our_palette)

gg_prevalence(injd, time_period = "monthly",
              title = "Monthly prevalence of sports injuries",
              line_mean = TRUE) +
  scale_fill_manual(values = our_palette)

gg_prevalence(injd, time_period = "monthly", by = "injury_type",
              title = "Monthly prevalence of each type of sports injury") +
  scale_fill_manual(values = our_palette)

gg_prevalence(injd, time_period = "monthly", by = "injury_type",
              title = "Monthly prevalence of each type of sports injury",
              line_mean = TRUE) +
  scale_fill_manual(values = our_palette)

# }
```
