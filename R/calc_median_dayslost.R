#' Calculate the median days lost
#'
#' Calculate the median of the days lost due to a sports-related health problem
#' (e.g. disease, injury).
#'
#' @inheritParams calc_ncases
#'
#' @return The median of the days lost. Either a numeric value (if overall
#'   \code{TRUE}) or a data frame indicating the median days lost per athlete.
#' @export
#'
#' @importFrom rlang ensym .data
#' @importFrom dplyr group_by ungroup summarise pull filter
#' @importFrom stats median
#'
#' @examples
#' calc_median_dayslost(injd)
#' calc_median_dayslost(injd, overall = FALSE)
#' calc_median_dayslost(injd, by = "injury_type")
calc_median_dayslost <- function(injd, by = NULL, overall = TRUE) {
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
      dplyr::summarise(median_dayslost = stats::median(.data$days_lost),
                       .groups = "keep") |>
      dplyr::ungroup() |>
      (\(x) if (is.null(by)) {
        x <- dplyr::pull(x, .data$median_dayslost)
        names(x) <- "median_dayslost"
        x
      } else {
        x
      })()
  } else {
    out <- injd |>
      dplyr::group_by(.data$person_id, {{ by }}) |>
      dplyr::summarise(median_dayslost = stats::median(.data$days_lost),
                       .groups = "keep") |>
      dplyr::ungroup()
  }

  return(out)
}

