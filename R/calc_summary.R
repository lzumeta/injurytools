#' Calculate summary statistics
#'
#' Calculate epidemiological summary statistics such as case (e.g. injury)
#' incidence and case burden (see Bahr et al. 2018), including total number of
#' cases, number of days lost due to this event, total time of exposure etc., by
#' means of a (widely used) Poisson method, negative binomial, zero-inflated
#' poisson or zero-inflated negative binomial, on a athlete and overall basis.
#'
#' @inheritParams calc_incidence
#' @param time_period TOWRITE!! TO THINK!!! see gg_time_trend also and prepare_all etc..
#'
#' @return A data frame comprising of overall or athlete-wise epidemiological
#'   summary statistics, that it's made up of the following columns:
#'   \itemize{
#'        \item \code{totalexpo}: total exposure that the athlete has been under
#'        risk of suffering a sports-related health problem.
#'        \item \code{ncases}: number of sports-related health problems suffered
#'        by the athlete or overall in the team/cohort over the given period
#'        specified by the \code{injd} data frame.
#'        \item \code{ndayslost}: number of days lost by the athlete or overall
#'        in the team/cohort due to the sports-related health problem over the
#'        given period specified by the \code{injd} data frame.
#'        \item \code{mean_dayslost}: average of number of days lost (i.e.
#'        \code{ndayslost}) athlete-wise or overall in the team/cohort.
#'        \item \code{median_dayslost}: median of number of days lost (i.e.
#'        \code{ndayslost}) athlete-wise or overall in the team/cohort.
#'        \item \code{qt25_dayslost} and \code{qt75_dayslost}: interquartile
#'        range of number of days lost (i.e. \code{ndayslost}) athlete-wise or
#'        overall in the team/cohort. \item \code{incidence}: case
#'        incidence rate, number of cases per unit of exposure. \item
#'        \code{burden}: case burden rate, number of days lost per unit of
#'        exposure.
#'        \item \code{incidence_sd} and \code{burden_sd}: estimated standard
#'        deviation, by the specified \code{method} argument, of case incidence
#'        (\code{incidence}) and case burden (\code{burden}). \item
#'        \code{incidence_lower} and \code{burden_lower}: lower bound of, for
#'        example, 95% confidence interval (if \code{conf_level = 0.95}) of case
#'        incidence (\code{incidence}) and case burden (\code{burden}).
#'        \item \code{incidence_upper} and \code{burden_upper}: the same (as
#'        above item) applies but for the upper bound.
#' }
#' Apart from this column names, they may further include these other columns
#' depending on the user's specifications to the function:
#' \itemize{
#'      \item \code{by}: only if it is specified as an argument to
#'        function.
#'      \item \code{percent_ncases}: percentage (%) of number of cases of
#'      that type relative to all types of cases (if \code{by}
#'      specified).
#'      \item \code{percent_dayslost}: percentage (%) of number of days lost
#'      because of cases of that type relative to the total number of days
#'      lost because of all types of cases (if \code{by}
#'      specified).
#' }
#'
#' @export
#' @references
#'  Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the burden
#'  of injuries and illnesses, not just their incidence. \emph{British Journal
#'  of Sports Medicine}, 52(16), 1018–1021.
#'  \url{https://doi.org/10.1136/bjsports-2017-098160}
#'
#'  Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L., ... &
#'  Andersen T. E. (2023). Football-specific extension of the IOC consensus
#'  statement: methods for recording and reporting of epidemiological data on
#'  injury and illness in sport 2020. \emph{British journal of sports
#'  medicine}.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr left_join select mutate_all
#' @importFrom rlang as_string ensym
#' @importFrom tidyselect everything all_of
#' @importFrom tidyr replace_na
#'
#' @examples
#' calc_summary(injd)
#' calc_summary(injd, overall = FALSE)
#' calc_summary(injd, by = "injury_type")
#' calc_summary(injd, by = "injury_type", overall = FALSE)
calc_summary <- function(injd, by = NULL, overall = TRUE,
                         method = c("poisson", "negbin", "zinfpois", "zinfnb"),
                         time_period = NULL,
                         conf_level = 0.95,
                         scale = TRUE,
                         quiet = FALSE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))
  if (missing(method)) method <- "poisson"

  incidence <- calc_incidence(injd, by = by, overall = overall, se = TRUE,
                              conf_level = conf_level, scale = scale,
                              quiet = quiet)
  burden    <- calc_burden(injd, by = by, overall = overall, se = TRUE,
                           conf_level = conf_level, scale = scale,
                           quiet = quiet)

  mean_dayslost   <- calc_mean_dayslost(injd, by = by, overall = overall)
  median_dayslost <- calc_median_dayslost(injd, by = by, overall = overall)
  iqr_dayslost    <- calc_iqr_dayslost(injd, by = by, overall = overall)

  if (!is.null(by)) {
    by <- rlang::ensym(by)
  }

  by_aux      <- if(!is.null(by)) rlang::as_string(by) else NULL
  athlete_aux <- if(!overall) "person_id" else NULL

  if (!is.null(by) & overall == TRUE) {
    incidence <- dplyr::mutate(incidence,
                               percent_ncases = round(.data$ncases * 100 / sum(.data$ncases), 2)) |>
      dplyr::select(tidyselect::all_of(by), "ncases", "percent_ncases", tidyselect::everything())
    burden    <- dplyr::mutate(burden,
                               percent_ndayslost = round(.data$ndayslost * 100 / sum(.data$ndayslost), 2)) |>
      dplyr::select(tidyselect::all_of(by), "ndayslost", "percent_ndayslost", tidyselect::everything())
  }

  if (is.null(by) & overall == TRUE) {
    mean_dayslost <- tibble::tibble(mean_dayslost)
    median_dayslost <- tibble::tibble(median_dayslost)
    iqr_dayslost    <- tibble::tibble(qt25_dayslost = iqr_dayslost[[1]],
                                      qt75_dayslost = iqr_dayslost[[2]])
    burden <- cbind(burden, mean_dayslost, median_dayslost, iqr_dayslost) |>
      dplyr::select("totalexpo", "ndayslost", "mean_dayslost",
                    "median_dayslost",
                    "qt25_dayslost", "qt75_dayslost",
                    tidyselect::everything())
  } else {
    burden <- dplyr::left_join(burden,
                               dplyr::left_join(mean_dayslost,
                                                dplyr::left_join(median_dayslost, iqr_dayslost,
                                                                 by = c(by_aux, athlete_aux)),
                                                by = c(by_aux, athlete_aux)),
                               by = c(by_aux, athlete_aux)) |>
      dplyr::select(tidyselect::all_of(athlete_aux),
                    tidyselect::all_of(by_aux),
                    "totalexpo", "ndayslost",
                    tidyselect::any_of("percent_ndayslost"),
                    "mean_dayslost",
                    "median_dayslost",
                    "qt25_dayslost", "qt75_dayslost",
                    tidyselect::everything())
  }
  out <- dplyr::left_join(incidence,
                          burden,
                          by = c("totalexpo", by_aux, athlete_aux)) |>
    dplyr::mutate_all(~tidyr::replace_na(.x, 0))
  return(out)
}
