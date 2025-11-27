# Changelog

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
