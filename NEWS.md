# injurytools 2.0.0

- The "nomenclature" now is changed:
    - player --> athlete;
    - injuries -> cases or/and sports-related health problem.
    - `gg_injriskmatrix()` -> `gg_riskmatrix()`
    - `gg_injphoto()` -> `gg_photo()`
    - `gg_injbarplot()` -> `gg_rank()` some new functionalities also added to these functions.

- `injsummary()` function has been split into smaller functions and its name now is: `injsummary()` -> `calc_summary()`. New (auxiliary) functions created:
       - `calc_ncases()`
       - `calc_ndayslost()`
       - `calc_exposure()`
       - `calc_mean_dayslost()`
       - `calc_median_dayslost()`
       - `calc_iqr_dayslost()`
       - `calc_incidence()`
       - `calc_burden()`.
 
 - `injprev()` has been modified and named `calc_prevalence()`. Also, `gg_injprev_polar()` has been modified and named `gg_prevalence()`.
 
 - `prepare_all()` has been modified: some attributes have been removed, and, in general, has been made more efficient/faster.
 
 - Created some utility and help functions.


# injurytools 1.0.3 

- `gg_injriskmatrix` now has `cont_max_x` and `cont_max_y` arguments to (optionally) control the ranges of contour lines and allows to pass on more arguments to `geom_contour()`, e.g. `bins = `, `binwidth = `, etc.

# injurytools 1.0.2 

- Fixes in injsummary(): exposure for injury type corrected and proper `conf_level`.
- Quit pipe operator %>% dependency.
- Added vignette for [modelling sports injury data (counts)](https://lzumeta.github.io/injurytools/articles/model-injury-data-i.html).
- Added vignette for [modelling sports injury data (events)](https://lzumeta.github.io/injurytools/articles/model-injury-data-ii.html)

# injurytools 1.0.1

- Maintenance update: fixes CRAN issues due to the new release of `dplyr`
- New feature of joins integrated

# injurytools 1.0.0 

- Initial release version to CRAN
