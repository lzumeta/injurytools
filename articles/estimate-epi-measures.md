# Estimate Measures of Injury Epidemiology

``` r
library(injurytools)
library(dplyr)      
library(knitr)      
library(kableExtra) 
```

Whenever one collects and prepares the data, the next natural step is to
summarize and explore these data. In this case, from a sports-applied
point of view, one wants to know e.g.: how many and what type of
injuries have occurred, how often they have occurred or what the load
has been.

This document shows convenient functions from `injurytools` to describe
sports injury data, in terms of measures used in sports injury
epidemiology (see Bahr and Holme ([2003](#ref-Bahr2003)) and Waldén et
al. ([2023](#ref-walden2023))). Below, these measures are explained and
then,
[`calc_summary()`](https://lzumeta.github.io/injurytools/reference/calc_summary.md)
and
[`calc_prevalence()`](https://lzumeta.github.io/injurytools/reference/calc_prevalence.md)
functions are illustrated.

## Measures of occurrence

### Rates

As Hodgson Phillips ([2000](#ref-Hodgson2000)) state,

*“Sports injuries occur when athletes are exposed to their given sport
and they occur under specific conditions, at a known time and place.”*

Thus, when attempting to describe the distribution of injuries it is
necessary to relate this to the population at risk over a specified time
period. This is why the fundamental unit of measurement is **rate**.

A rate is a measure that consists of a denominator and a numerator over
a period of time. Denominator data can be a number of different things
(e.g. number of minutes trained/played, number of matches played). As
such, it reflects the speed at which new “injury-related” events occur.

#### Injury incidence rate

**Injury incidence rate** is the number of new injury cases ($I$) per
unit of player-exposure time, i.e.

$$I_{r} = \frac{I}{\Delta T}$$ where $\Delta T$ is the total time under
risk of the study population.

#### Injury burden rate

**Injury burden rate** is the number of days lost ($n_{d}$) per unit of
player-exposure time, i.e.

$$I_{br} = \frac{n_{d}}{\Delta T}$$ where $\Delta T$ is the total time
under risk of the study population.

### Prevalence

**Prevalence**, period prevalence, is a proportion that refers to the
number of players that have reported the injury of interest ($X$)
divided by the total player-population at risk at any time during the
specified period of time ($\Delta T$ time window). This includes players
who already had the injury at the start of the time period as well as
those who suffer it during that period.

$$P = \frac{X}{N}$$ where $X$ is the number of injury cases and $N$ is
the total number of players in the study at any point in the time window
$\Delta T$.

Please, see also the
[Notes](https://lzumeta.github.io/injurytools/articles/estimate-epi-measures.html#notes)
section below.

## In practice

Again, as in the
[prepare-data](https://lzumeta.github.io/injurytools/articles/prepare-injury-data.html)
vignette, we use the data sets available from the `injurytools` package:
data from Liverpool Football Club male’s first team players over two
consecutive seasons, 2017-2018 and 2018-2019, scrapped from
<https://www.transfermarkt.com/> website[¹](#fn1).

### - `calc_summary()`

Prepare data

``` r
df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
                            date = "year", time_expo = "minutes_played")
df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
                            date_injured = "from", date_recovered = "until")
injd         <- prepare_all(data_exposures = df_exposures,
                            data_injuries  = df_injuries,
                            exp_unit = "matches_minutes")
```

Now, the preprocessed data are passed to
[`calc_summary()`](https://lzumeta.github.io/injurytools/reference/calc_summary.md)
to calculate injury summary statistics overall or playerwise:

``` r
df_summary   <- calc_summary(injd)
#> Warning in summary_unit(unit, out, quiet): 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> Warning in summary_unit(unit, out, quiet): 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
df_summary_p <- calc_summary(injd, overall = FALSE)
#> Warning in summary_unit(unit, out, quiet): 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
#> Warning in summary_unit(unit, out, quiet): 
#>  Exposure time unit is matches_minutes
#>  Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)
```

We notice that some warning messages pop up (unless `quiet = TRUE`).
They are displayed to make it clear what the exposure time unit
is[²](#fn2).

To present the results in a more tidier and comprehensible way (instead
of **R** code styled output) the following can be done:

``` r
# the 'playerwise' data frame
df_summary_p
```

Code to format the table

``` r
# format the 'playerwise' data frame for output as a table
df_summary_p |> 
  arrange(desc(incidence)) |> # sort by decreasing order of incidence
  mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |> 
  select("person_id", "ncases", "ndayslost", "mean_dayslost",
         "median_dayslost", "iqr_dayslost", "totalexpo",
         "incidence", "burden") |> 
  head(10) |>
  kable(digits = 2, col.names = c("Player", "N injuries", "N days lost", 
                                  "Mean days lost", "Median days lost", 
                                  "IQR days lost",
                                  "Total exposure", "Incidence", "Burden"))
```

| Player            | N injuries | N days lost | Mean days lost | Median days lost | IQR days lost | Total exposure | Incidence |  Burden |
|:------------------|-----------:|------------:|---------------:|-----------------:|:--------------|---------------:|----------:|--------:|
| adam-lallana      |          6 |         302 |          50.33 |             44.0 | 29.5 - 56.25  |            700 |     77.14 | 3882.86 |
| daniel-sturridge  |          3 |         122 |          40.67 |             51.0 | 33.5 - 53     |            927 |     29.13 | 1184.47 |
| divock-origi      |          1 |           5 |           5.00 |              5.0 | 5 - 5         |            366 |     24.59 |  122.95 |
| philippe-coutinho |          3 |          62 |          20.67 |             25.0 | 18.5 - 25     |           1117 |     24.17 |  499.55 |
| naby-keita        |          3 |          89 |          29.67 |             20.0 | 19 - 35.5     |           1393 |     19.38 |  575.02 |
| dejan-lovren      |          6 |         160 |          26.67 |             17.0 | 10 - 32.25    |           3109 |     17.37 |  463.17 |
| jordan-henderson  |          8 |          91 |          11.38 |              7.5 | 6.25 - 12.75  |           4154 |     17.33 |  197.16 |
| xherdan-shaqiri   |          2 |          67 |          33.50 |             33.5 | 23.25 - 43.75 |           1057 |     17.03 |  570.48 |
| fabinho           |          3 |          22 |           7.33 |              9.0 | 5.5 - 10      |           2013 |     13.41 |   98.36 |
| james-milner      |          5 |          48 |           9.60 |             11.0 | 7 - 12        |           3548 |     12.68 |  121.76 |

We used [RMarkdown](https://rmarkdown.rstudio.com/), in particular
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) function,
to report these tables in this way.

``` r
# the 'overall' data frame
df_summary
```

Code to format the table

``` r
# format the table of total incidence and burden (main columns)
df_summary |> 
  mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |> 
  select("ncases", "ndayslost", "mean_dayslost", "median_dayslost",
         "iqr_dayslost", "totalexpo", "incidence", "burden") |> 
  data.frame(row.names = "TOTAL") |> 
  kable(digits = 2,
        col.names = c("N injuries", "N days lost", "Mean days lost",
                      "Median days lost", "IQR days lost",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)
```

|       | N injuries | N days lost | Mean days lost | Median days lost | IQR days lost | Total exposure | Incidence | Burden |
|:------|-----------:|------------:|---------------:|-----------------:|:--------------|---------------:|----------:|-------:|
| TOTAL |         82 |        2049 |          24.99 |               12 | 6 - 25        |          74690 |      9.88 |  246.9 |

Note that to provide numbers that are easy to interpret and to avoid
small decimals, injury incidence and injury burden are reported ‘per 100
player-match exposure’. As in this example exposure time is **minutes
played in matches**, we multiply the rates by 90\*100 (i.e. 90 minutes
lasts a football match). The reported incidence rate is estimated by
${\widehat{I}}_{r} = \frac{82}{74690} \times 90 \times 100$.

Code to format the table

``` r
# format the table of total incidence and burden (point + ci estimates)
dfs_cis <- df_summary |> 
  select(starts_with("incid"), starts_with("burd")) |> 
  data.frame(row.names = "TOTAL")
dfs_cis$ci_incidence <- paste0("[",  round(dfs_cis$incidence_lower, 1),
                                        ", ", round(dfs_cis$incidence_upper, 1), "]")
dfs_cis$ci_burden    <- paste0("[",  round(dfs_cis$burden_lower, 1),
                                        ", ", round(dfs_cis$burden_upper, 1), "]")

conf_level <- 0.95 * 100

dfs_cis |> 
  select(1, 9, 5, 10) |> 
  kable(digits = 2,
        col.names = c("Incidence", paste0(conf_level, "% CI for \\(I_r\\)"), 
                      "Burden",    paste0(conf_level, "% CI for \\(I_{br}\\)")))
```

|       | Incidence | 95% CI for $I_{r}$ | Burden | 95% CI for $I_{br}$ |
|:------|----------:|:-------------------|-------:|:--------------------|
| TOTAL |      9.88 | \[7.7, 12\]        |  246.9 | \[236.2, 257.6\]    |

Players with the highest injury incidence rate (all type of injuries)
were Adam Lallana and Daniel Sturridge with 77.1 and 29.1 injuries per
100 player-matches respectively. The teams overall injury incidence was
of 9.9 injuries per 100 player-matches and the injury burden of 246.9
days lost per 100 player-matches.

These summaries can be done by type of injury:

``` r
dfs_pertype <- calc_summary(injd, by = "injury_type", quiet = T)
```

These are the results of the team regarding injury incidence and injury
burden by type of injury:

``` r
dfs_pertype
```

Code to format the table

``` r
dfs_pertype |> 
  select(1:5, 9:15) |> 
  mutate(ncases2 = paste0(ncases, " (", percent_ncases, ")"),
         ndayslost2 = paste0(ndayslost, " (", percent_ndayslost, ")"),
         iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost),
         median_dayslost2 = paste0(median_dayslost, " (", iqr_dayslost, ")")) |> 
  select(1, 13:14, 16, 4:5, 12) |> 
  arrange(desc(burden)) |> 
  kable(digits = 2,
        col.names = c("Type of injury", "N injuries (%)", "N days lost (%)",
                      "Median days lost (IQR)",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)
```

|     | Type of injury | N injuries (%) | N days lost (%) | Median days lost (IQR) | Total exposure | Incidence | Burden |
|:----|:---------------|:---------------|:----------------|:-----------------------|---------------:|----------:|-------:|
| 1   | Muscle         | 25 (30.49)     | 735 (35.87)     | 21 (12 - 36)           |          74690 |      3.01 |  88.57 |
| 2   | Ligament       | 9 (10.98)      | 596 (29.09)     | 28 (7 - 54)            |          74690 |      1.08 |  71.82 |
| 3   | Unknown        | 21 (25.61)     | 332 (16.2)      | 7 (4 - 18)             |          74690 |      2.53 |  40.01 |
| 4   | Concussion     | 16 (19.51)     | 213 (10.4)      | 10.5 (5.75 - 14.5)     |          74690 |      1.93 |  25.67 |
| 5   | Bone           | 11 (13.41)     | 173 (8.44)      | 9 (4.5 - 16.5)         |          74690 |      1.33 |  20.85 |

### - `calc_prevalence()`

Prepare data

``` r
df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
                            date = "year", time_expo = "minutes_played")
df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
                            date_injured = "from", date_recovered = "until")
injd         <- prepare_all(data_exposures = df_exposures,
                            data_injuries  = df_injuries,
                            exp_unit = "matches_minutes")
```

We calculate the injury prevalence and the proportions of injury-free
players on a season basis:

``` r
prev_table1 <- calc_prevalence(injd, time_period = "season")
prev_table1
#> # A tibble: 4 × 5
#>   season           status        n n_athlete  prop
#>   <fct>            <fct>     <int>     <int> <dbl>
#> 1 season 2017/2018 Available     7        23  30.4
#> 2 season 2017/2018 Injured      16        23  69.6
#> 3 season 2018/2019 Available     2        19  10.5
#> 4 season 2018/2019 Injured      17        19  89.5
```

Making use of
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html):

``` r
kable(prev_table1,
      col.names = c("Season", "Status", "N", "Total", "%"))
```

| Season           | Status    |   N | Total |    % |
|:-----------------|:----------|----:|------:|-----:|
| season 2017/2018 | Available |   7 |    23 | 30.4 |
| season 2017/2018 | Injured   |  16 |    23 | 69.6 |
| season 2018/2019 | Available |   2 |    19 | 10.5 |
| season 2018/2019 | Injured   |  17 |    19 | 89.5 |

Overall, there were more injured players in the 18-19 season than in the
previous season. Let’s calculate it monthly:

``` r
prev_table2 <- calc_prevalence(injd, time_period = "monthly")

## compare two seasons July and August
prev_table2 |>
  group_by(season) |> 
  slice(1:4)
#> # A tibble: 8 × 6
#> # Groups:   season [2]
#>   season           month status        n n_athlete  prop
#>   <fct>            <fct> <fct>     <int>     <int> <dbl>
#> 1 season 2017/2018 Jul   Available    21        23  91.3
#> 2 season 2017/2018 Jul   Injured       2        23   8.7
#> 3 season 2017/2018 Aug   Available    18        23  78.3
#> 4 season 2017/2018 Aug   Injured       5        23  21.7
#> 5 season 2018/2019 Jul   Available    16        19  84.2
#> 6 season 2018/2019 Jul   Injured       3        19  15.8
#> 7 season 2018/2019 Aug   Available    15        19  78.9
#> 8 season 2018/2019 Aug   Injured       4        19  21.1


## compare two seasons January and February
prev_table2 |>
  group_by(season) |> 
  slice(13:16)
#> # A tibble: 8 × 6
#> # Groups:   season [2]
#>   season           month status        n n_athlete  prop
#>   <fct>            <fct> <fct>     <int>     <int> <dbl>
#> 1 season 2017/2018 Jan   Available    18        23  78.3
#> 2 season 2017/2018 Jan   Injured       5        23  21.7
#> 3 season 2017/2018 Feb   Available    21        23  91.3
#> 4 season 2017/2018 Feb   Injured       2        23   8.7
#> 5 season 2018/2019 Jan   Available     9        19  47.4
#> 6 season 2018/2019 Jan   Injured      10        19  52.6
#> 7 season 2018/2019 Feb   Available    12        19  63.2
#> 8 season 2018/2019 Feb   Injured       7        19  36.8
```

Looking at monthly basis, there were more differences w.r.t. player
availability in Liverpool FC 1st male team, during the winter
January/February months. More injured players in the 18-19 season.

``` r
prev_table3 <- calc_prevalence(injd, time_period = "monthly", by = "injury_type")
```

Tidy up

``` r
## season 1
prev_table3 |> 
  filter(season == "season 2017/2018", month == "Jan") |> 
  kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
        caption = "Season 2017/2018") |> 
  kable_styling(full_width = FALSE, position = "float_left")
## season 2
prev_table3 |> 
  filter(season == "season 2018/2019", month == "Jan") |> 
  kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
        caption = "Season 2018/2019") |> 
  kable_styling(full_width = FALSE, position = "left")
```

| Season           | Month | Status    |   N | Total |    % |
|:-----------------|:------|:----------|----:|------:|-----:|
| season 2017/2018 | Jan   | Available |  18 |    23 | 78.3 |
| season 2017/2018 | Jan   | Ligament  |   1 |    23 |  4.3 |
| season 2017/2018 | Jan   | Muscle    |   3 |    23 | 13.0 |
| season 2017/2018 | Jan   | Unknown   |   1 |    23 |  4.3 |

Season 2017/2018

| Season           | Month | Status     |   N | Total |    % |
|:-----------------|:------|:-----------|----:|------:|-----:|
| season 2018/2019 | Jan   | Available  |   9 |    19 | 47.4 |
| season 2018/2019 | Jan   | Bone       |   1 |    19 |  5.3 |
| season 2018/2019 | Jan   | Concussion |   2 |    19 | 10.5 |
| season 2018/2019 | Jan   | Ligament   |   1 |    19 |  5.3 |
| season 2018/2019 | Jan   | Muscle     |   3 |    19 | 15.8 |
| season 2018/2019 | Jan   | Unknown    |   4 |    19 | 21.1 |

Season 2018/2019

## Further implementation

In the near future there will be available the negative binomial
(`method = "negbin"` argument), zero-inflated poisson (“`zinfpois`”) and
zero-inflated negative binomial (`"zinfnb"`) methods in
[`calc_incidence()`](https://lzumeta.github.io/injurytools/reference/calc_incidence.md)
and
[`calc_burden()`](https://lzumeta.github.io/injurytools/reference/calc_burden.md)
functions.

Finally, this document shows how to perform descriptive analyses for
injury epidemiology, but naturally, following these analyses further
statistical inferences or multivariate regression analyses may be chosen
to infer about the player’s/athletes population properties (e.g. to test
whether there are differences between the injury incidence rates of two
cohorts) or to evaluate the influence of independent factors
(e.g. previous injuries, workload) on the injuries occurred.

## Notes

**NOTE 1:** as Bahr, Clarsen, and Ekstrand ([2018](#ref-Bahr2018))
state, injury incidence (likelihood) and injury burden (severity) should
be reported and assessed in conjunction rather than in isolation. In
this regard, see the risk matrix plot provided by `gg_injriskmatrix()`
function.

**NOTE 2:** neither injury incidence ($I_{r}$) nor injury burden
($I_{br}$) are ratios, and they are not interpreted as a probability;
they are rates and their unit (person-time)$^{- 1}$ (e.g. per 1000h of
player-exposure, per player-season etc.).

**NOTE 3:** as Waldén et al. ([2023](#ref-walden2023)) point out,
incidence-based measures that provide a standardized time window for the
population at risk (e.g., injuries per hour) are preferred over measures
in which the time at risk varies among individuals (e.g., injuries per
athletic exposure, injuries per number of matches). This preference for
measures with standardized time scales aids in the comparison of
statistics across different cohorts and sports.

## References

Bahr, R, B Clarsen, and J Ekstrand. 2018. “Why We Should Focus on the
Burden of Injuries and Illnesses, Not Just Their Incidence.” *British
Journal of Sports Medicine* 52 (16): 1018–21.
<https://doi.org/10.1136/bjsports-2017-098160>.

Bahr, R, and I Holme. 2003. “Risk Factors for Sports Injuries—a
Methodological Approach.” *British Journal of Sports Medicine* 37 (5):
384–92. <https://doi.org/10.1136/bjsm.37.5.384>.

Hodgson Phillips, L. 2000. “Sports Injury Incidence.” *British Journal
of Sports Medicine* 34 (2): 133–36.
<https://doi.org/10.1136/bjsm.34.2.133>.

Waldén, Markus, Margo Mountjoy, Alan McCall, Andreas Serner, Andrew
Massey, Johannes L Tol, Roald Bahr, et al. 2023. “Football-Specific
Extension of the IOC Consensus Statement: Methods for Recording and
Reporting of Epidemiological Data on Injury and Illness in Sport 2020.”
*British Journal of Sports Medicine*.
<https://doi.org/10.1136/bjsports-2022-106405>.

------------------------------------------------------------------------

1.  These data sets are provided for illustrative purposes. We warn that
    they might not be accurate and could potentially include
    discrepancies or incomplete information compared to what actually
    occurred.

2.  Something that it is important to be aware of.
