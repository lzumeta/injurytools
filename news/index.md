# Changelog

## injurytools 2.0.0

- The “nomenclature” now is changed:

  - player –\> athlete;
  - injuries -\> cases or/and sports-related health problem.
  - `gg_injriskmatrix()` -\>
    [`gg_riskmatrix()`](https://lzumeta.github.io/injurytools/reference/gg_riskmatrix.md)
  - `gg_injphoto()` -\>
    [`gg_photo()`](https://lzumeta.github.io/injurytools/reference/gg_photo.md)
  - `gg_injbarplot()` -\>
    [`gg_rank()`](https://lzumeta.github.io/injurytools/reference/gg_rank.md)
    some new functionalities also added to these functions.

- `injsummary()` function has been split into smaller functions and its
  name now is: `injsummary()` -\>
  [`calc_summary()`](https://lzumeta.github.io/injurytools/reference/calc_summary.md).
  New (auxiliary) functions created: -
  [`calc_ncases()`](https://lzumeta.github.io/injurytools/reference/calc_ncases.md) -
  [`calc_ndayslost()`](https://lzumeta.github.io/injurytools/reference/calc_ndayslost.md) -
  [`calc_exposure()`](https://lzumeta.github.io/injurytools/reference/calc_exposure.md) -
  [`calc_mean_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_mean_dayslost.md) -
  [`calc_median_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_median_dayslost.md) -
  [`calc_iqr_dayslost()`](https://lzumeta.github.io/injurytools/reference/calc_iqr_dayslost.md) -
  [`calc_incidence()`](https://lzumeta.github.io/injurytools/reference/calc_incidence.md) -
  [`calc_burden()`](https://lzumeta.github.io/injurytools/reference/calc_burden.md).

- `injprev()` has been modified and named
  [`calc_prevalence()`](https://lzumeta.github.io/injurytools/reference/calc_prevalence.md).
  Also, `gg_injprev_polar()` has been modified and named
  [`gg_prevalence()`](https://lzumeta.github.io/injurytools/reference/gg_prevalence.md).

- [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
  has been modified: some attributes have been removed, and, in general,
  has been made more efficient/faster.

- Created some utility and help functions.

## injurytools 1.0.3

CRAN release: 2023-11-14

- `gg_injriskmatrix` now has `cont_max_x` and `cont_max_y` arguments to
  (optionally) control the ranges of contour lines and allows to pass on
  more arguments to
  [`geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html),
  e.g. `bins =`, `binwidth =`, etc.

## injurytools 1.0.2

CRAN release: 2023-09-27

- Fixes in injsummary(): exposure for injury type corrected and proper
  `conf_level`.
- Quit pipe operator %\>% dependency.
- Added vignette for [modelling sports injury data
  (counts)](https://lzumeta.github.io/injurytools/articles/model-injury-data-i.html).
- Added vignette for [modelling sports injury data
  (events)](https://lzumeta.github.io/injurytools/articles/model-injury-data-ii.html)

## injurytools 1.0.1

CRAN release: 2023-02-01

- Maintenance update: fixes CRAN issues due to the new release of
  `dplyr`
- New feature of joins integrated

## injurytools 1.0.0

CRAN release: 2023-01-26

- Initial release version to CRAN
