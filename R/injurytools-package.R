#' \pkg{injurytools} package
#'
#' @keywords internal
#' @docType package
#' @name injurytools
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate checkLogical
#' @importFrom checkmate expect_data_frame
#' @importFrom checkmate expect_subset
#' @importFrom forcats fct_reorder
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom purrr map_dfr
#' @importFrom stats relevel
#' @importFrom stringr str_to_title
#' @importFrom tidyr replace_na
#' @importFrom withr with_locale
## usethis namespace: end
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# ## see: https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/
# utils::globalVariables(c(".", "player", "t0", "tf", "date_injured",
#                          "date_recovered","event", "time_expo", "status",
#                          "enum", "tstart_min", "tstop_min"))
utils::globalVariables(".")
