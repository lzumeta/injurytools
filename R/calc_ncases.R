#' Calculate number of cases
#'
#' Calculate the number of sports-related cases (e.g. injuries) that occurred in
#' a cohort during a period.
#'
#' @param injd \code{injd} \strong{S3} object (see
#'   \code{\link[=prepare_all]{prepare_all()}}).
#' @param by Character specifying the name of the column according to which
#'   compute summary statistics. It should refer to a (categorical) variable
#'   that describes a grouping factor (e.g. "type of case or injury", "injury
#'   location", "sports club"). Optional, defaults to \code{NULL}.
#' @param overall Logical, whether to calculate overall (for all the cohort) or
#'   athlete-wise summary statistic (i.e. number of cases per cohort of per
#'   athlete). Defaults to \code{TRUE}.
#'
#' @return The number of cases. Either a numeric value (if overall \code{TRUE})
#'   or a data frame indicating the number of cases per athlete.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate group_by ungroup summarise pull filter
#' @importFrom tidyr complete replace_na
#'
#' @examples
#' calc_ncases(injd)
#' calc_ncases(injd, overall = FALSE)
#' calc_ncases(injd, by = "injury_type")
calc_ncases <- function(injd, by = NULL, overall = TRUE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))

  if (!is.null(by)) {
    by <- rlang::ensym(by)
    injd <- injd |>
      tidyr::complete(.data$person_id, {{ by }}) |>
      dplyr::mutate(status = tidyr::replace_na(.data$status, 0)) |>
      dplyr::filter(!is.na({{ by }}))
  }

  if (overall) {
    out <- injd  |>
      dplyr::group_by({{ by }}) |>
      dplyr::summarise(ncases  = sum(.data$status),
                       .groups = "keep") |>
      dplyr::ungroup() |>
      (\(x) if (is.null(by)) {
        x <- dplyr::pull(x, .data$ncases)
        names(x) <- "ncases"
        x
      } else {
        x
      })()
  } else {
    out <- injd |>
      dplyr::group_by(.data$person_id, {{ by }}) |>
      dplyr::summarise(ncases  = sum(.data$status),
                       .groups = "keep") |>
      ungroup()
  }

  return(out)
}

