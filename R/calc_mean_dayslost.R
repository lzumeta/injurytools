#' Calculate the mean days lost
#'
#' Calculate the mean of the days lost due to a sports-related health problem
#' (e.g. disease, injury) in a cohort.
#'
#' @inheritParams calc_ncases
#'
#' @return The mean of the days lost. Either a numeric value (if overall
#'   \code{TRUE}) or a data frame indicating the mean days lost per athlete.
#' @export
#'
#' @importFrom rlang ensym .data
#' @importFrom dplyr group_by ungroup summarise pull filter
#'
#' @examples
#' calc_mean_dayslost(injd)
#' calc_mean_dayslost(injd, overall = FALSE)
#' calc_mean_dayslost(injd, by = "injury_type")
calc_mean_dayslost <- function(injd, by = NULL, overall = TRUE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))

  if (!is.null(by)) {
    by <- rlang::ensym(by)
  }

  injd <- injd |>
    dplyr::filter(.data$status == 1)

  if (overall) {
    out <- injd  |>
      dplyr::group_by({{ by }}) |>
      dplyr::summarise(mean_dayslost = mean(.data$days_lost),
                       .groups = "keep") |>
      dplyr::ungroup() |>
      (\(x) if (is.null(by)) {
        x <- dplyr::pull(x, .data$mean_dayslost)
        names(x) <- "mean_dayslost"
        x
      } else {
        x
      })()
  } else {
    out <- injd |>
      dplyr::group_by(.data$person_id, {{ by }}) |>
      dplyr::summarise(mean_dayslost = mean(.data$days_lost),
                       .groups = "keep") |>
      dplyr::ungroup()
  }

  return(out)
}
