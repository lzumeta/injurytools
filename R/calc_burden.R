#' Calculate case burden rate
#'
#' Calculate the case burden rate of a sports-related health problem (e.g.
#' disease, injury) in a cohort.
#'
#' @inheritParams calc_incidence
#'
#' @return The case burden rate. Either a numeric value (if overall
#'   \code{TRUE}) or a data frame indicating the case burden rate per
#'   athlete.
#' @export
#'
#' @references
#'  Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the burden
#'  of injuries and illnesses, not just their incidence. \emph{British Journal of
#'  Sports Medicine}, 52(16), 1018–1021.
#'  \url{https://doi.org/10.1136/bjsports-2017-098160}
#'
#'  Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L.,
#'  ... & Andersen T. E. (2023). Football-specific extension of the IOC
#'  consensus statement: methods for recording and reporting of epidemiological
#'  data on injury and illness in sport 2020. \emph{British journal of sports
#'  medicine}.
#'
#' @importFrom dplyr mutate mutate_all left_join
#' @importFrom stats qnorm
#' @importFrom tibble as_tibble
#' @importFrom rlang as_string
#' @importFrom tidyr replace_na
#'
#' @examples
#' calc_burden(injd)
#' calc_burden(injd, overall = FALSE)
#' calc_burden(injd, by = "injury_type")
calc_burden <- function(injd, by = NULL, overall = TRUE,
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

  ndayslost <- calc_ndayslost(injd, by = by, overall = overall)
  totalexpo <- calc_exposure(injd, by = by, overall = overall, quiet = quiet)

  if (!is.null(by)) {
    by <- rlang::ensym(by)
  }

  if (is.null(by) & overall == TRUE) {
    out <- cbind(totalexpo, ndayslost) |> tibble::as_tibble()
  } else {
    by_aux      <- if(!is.null(by)) rlang::as_string(by) else NULL
    athlete_aux <- if(!overall) "person_id" else NULL
    out <- dplyr::left_join(totalexpo, ndayslost, by = c(by_aux, athlete_aux))
  }
  out <- out |>
    dplyr::mutate(burden = ndayslost/totalexpo)

  if (se) {
    ## method (point estimate and standard error)
    minusalphahalf <- conf_level + (1 - conf_level) / 2
    if (method == "poisson") { ## assuming that the number of injuries follows a poisson and basing on CLT
      out <- out |>
        dplyr::mutate(burden_sd = sqrt(.data$burden / .data$totalexpo),
                      burden_lower = .data$burden - stats::qnorm(minusalphahalf) * .data$burden_sd,
                      burden_upper = .data$burden + stats::qnorm(minusalphahalf) * .data$burden_sd)
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
