#' Calculate number of days lost
#'
#' Calculate the number of days lost due to a sports-related health problem
#' (e.g. injuries) in a cohort during a period.
#'
#' @inheritParams calc_ncases
#'
#' @return The number of days lost. Either a numeric value (if overall \code{TRUE})
#'   or a data frame indicating the number of cases per athlete.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate group_by ungroup summarise pull filter
#' @importFrom tidyr complete replace_na
#'
#' @examples
#' calc_ndayslost(injd)
#' calc_ndayslost(injd, overall = FALSE)
#' calc_ndayslost(injd, by = "injury_type")
calc_ndayslost <- function(injd, by = NULL, overall = TRUE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))

  if (!is.null(by)) {
    by <- rlang::ensym(by)
    injd <- injd |>
      tidyr::complete(.data$person_id, {{ by }}) |>
      dplyr::mutate(days_lost = tidyr::replace_na(.data$days_lost, 0)) |>
      dplyr::filter(!is.na({{ by }}))
  }

  if (overall) {
    out <- injd  |>
      dplyr::group_by({{ by }}) |>
      dplyr::summarise(ndayslost  = sum(.data$days_lost),
                       .groups = "keep") |>
      dplyr::ungroup() |>
      (\(x) if (is.null(by)) {
        x <- dplyr::pull(x, .data$ndayslost)
        names(x) <- "ndayslost"
        x
      } else {
        x
      })()
  } else {
    out <- injd |>
      dplyr::group_by(.data$person_id, {{ by }}) |>
      dplyr::summarise(ndayslost  = sum(.data$days_lost),
                       .groups = "keep") |>
      ungroup()
  }

  return(out)
}
