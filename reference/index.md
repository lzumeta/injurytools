# Package index

## Prepare sports injury and illness data

Functions that preprocess and prepare these type of data in a
standardized format.

- [`prepare_inj()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  [`prepare_exp()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  : Prepare data in a standardized format

## Compute injury summary statistics

Functions to estimate sports injury summary statistics.

- [`calc_summary()`](https://lzumeta.github.io/injurytools/reference/calc_summary.md)
  : Calculate summary statistics
- [`calc_prevalence()`](https://lzumeta.github.io/injurytools/reference/calc_prevalence.md)
  : Calculate prevalence proportion
- [`calc_ncases()`](https://lzumeta.github.io/injurytools/reference/calc_ncases.md)
  : Calculate number of cases
- [`calc_ndayslost()`](https://lzumeta.github.io/injurytools/reference/calc_ndayslost.md)
  : Calculate number of days lost
- [`calc_mean_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_mean_dayslost.md)
  : Calculate the mean days lost
- [`calc_median_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_median_dayslost.md)
  : Calculate the median days lost
- [`calc_iqr_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_iqr_dayslost.md)
  : Calculate the interquartile range days lost
- [`calc_exposure()`](https://lzumeta.github.io/injurytools/reference/calc_exposure.md)
  : Calculate the exposure time
- [`calc_incidence()`](https://lzumeta.github.io/injurytools/reference/calc_incidence.md)
  : Calculate case incidence rate
- [`calc_burden()`](https://lzumeta.github.io/injurytools/reference/calc_burden.md)
  : Calculate case burden rate

## Visualize sports injury data

Functions to visualize relevant information in sports injury data.

- [`gg_photo()`](https://lzumeta.github.io/injurytools/reference/gg_photo.md)
  : Plot injuries and illnesses over the follow-up period
- [`gg_riskmatrix()`](https://lzumeta.github.io/injurytools/reference/gg_riskmatrix.md)
  : Plot risk matrices
- [`gg_prevalence()`](https://lzumeta.github.io/injurytools/reference/gg_prevalence.md)
  : Plot bar plots representing players' prevalence
- [`gg_rank()`](https://lzumeta.github.io/injurytools/reference/gg_rank.md)
  : Plot athlete's health problem incidence or burden ranking

## Data sets

Internal data sets to illustrate `injurytools` functionalities.

- [`raw_df_exposures`](https://lzumeta.github.io/injurytools/reference/raw_df_exposures.md)
  : Minimal example of exposure data

- [`raw_df_injuries`](https://lzumeta.github.io/injurytools/reference/raw_df_injuries.md)
  : Minimal example of injury data

- [`injd`](https://lzumeta.github.io/injurytools/reference/injd.md) :

  Example of an `injd` object

## Utilities

Utility functions.

- [`date2season()`](https://lzumeta.github.io/injurytools/reference/date2season.md)
  : Get the season

- [`season2year()`](https://lzumeta.github.io/injurytools/reference/season2year.md)
  : Get the year

- [`cut_injd()`](https://lzumeta.github.io/injurytools/reference/cut_injd.md)
  : Cut the range of the follow-up

- [`is_injd()`](https://lzumeta.github.io/injurytools/reference/is_injd.md)
  :

  Check if an object is of class `injd`

- [`get_data_followup()`](https://lzumeta.github.io/injurytools/reference/get_data_followup.md)
  : Extract follow-up data frame

- [`get_data_injuries()`](https://lzumeta.github.io/injurytools/reference/get_data_injuries.md)
  : Extract injury/illness data frame

- [`get_data_exposures()`](https://lzumeta.github.io/injurytools/reference/get_data_exposures.md)
  : Extract exposures data frame
