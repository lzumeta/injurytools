#' Calculate case incidence rate
#'
#' Calculate the case incidence rate of a sports-related health problem (e.g.
#' disease, injury) in a cohort.
#'
#' @inheritParams calc_ncases
#' @param method Method to estimate the incidence (burden) rate. One of
#'   "poisson", "negbin", "zinfpois" or "zinfnb"; that stand for Poisson method,
#'   negative binomial method, zero-inflated Poisson and zero-inflated negative
#'   binomial.
#' @param se Logical, whether to calculate the confidence interval related to
#'   the rate.
#' @param conf_level Confidence level (defaults to 0.95).
#' @param scale Logical, whether to transform the incidence and burden rates
#'   output according to the unit of exposure (defaults to \code{TRUE}).
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return The case incidence rate. Either a numeric value (if overall
#'   \code{TRUE}) or a data frame indicating the case incidence rate per
#'   athlete.
#' @export
#'
#' @references
#'  Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the burden
#'  of injuries and illnesses, not just their incidence. \emph{British Journal of
#'  Sports Medicine}, 52(16), 1018–1021.
#'  \doi{10.1136/bjsports-2017-098160}
#'
#'  Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L., ... &
#'  Andersen T. E. (2023). Football-specific extension of the IOC consensus
#'  statement: methods for recording and reporting of epidemiological
#'  data on injury and illness in sport 2020. \emph{British journal of sports
#'  medicine}.
#'
#' @importFrom dplyr mutate mutate_all left_join
#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @importFrom rlang as_string .data ensym
#' @importFrom tidyr replace_na
#'
#' @examples
#' calc_incidence(injd)
#' calc_incidence(injd, overall = FALSE)
#' calc_incidence(injd, by = "injury_type")
#' calc_incidence(injd, by = "injury_type", scale = FALSE)
calc_incidence <- function(injd, by = NULL, overall = TRUE,
                           method = c("poisson", "negbin", "zinfpois", "zinfnb"),
                           se = TRUE,
                           conf_level = 0.95,
                           scale = TRUE,
                           quiet = FALSE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))
  assert(checkLogical(se))
  if (missing(method)) method <- "poisson"

  ncases    <- calc_ncases(injd, by = by, overall = overall)
  totalexpo <- calc_exposure(injd, by = by, overall = overall, quiet = quiet)

  if (!is.null(by)) {
    by <- rlang::ensym(by)
  }

  if (is.null(by) & overall == TRUE) {
    out <- cbind(totalexpo, ncases) |> tibble::as_tibble()
  } else {
    by_aux      <- if(!is.null(by)) rlang::as_string(by) else NULL
    athlete_aux <- if(!overall) "person_id" else NULL
    out <- dplyr::left_join(totalexpo, ncases, by = c(by_aux, athlete_aux))
  }
  out <- out |>
    dplyr::mutate(incidence = ncases/totalexpo)

  if (se) {
    ## method (point estimate and standard error)
    minusalphahalf <- conf_level + (1 - conf_level) / 2
    if (method == "poisson") { ## assuming that the number of injuries follows a poisson and basing on CLT
      out <- out |>
        dplyr::mutate(incidence_sd = sqrt(.data$incidence / .data$totalexpo),
                      incidence_lower = .data$incidence - stats::qnorm(minusalphahalf) * .data$incidence_sd,
                      incidence_upper = .data$incidence + stats::qnorm(minusalphahalf) * .data$incidence_sd)
    }
  }
  out <- out |>
    dplyr::mutate_all(~tidyr::replace_na(.x, 0))
  if (scale) {
    unit <- attr(injd, "unit_exposure")
    out  <- summary_unit(unit, out, quiet)
  }
  return(out)
}
