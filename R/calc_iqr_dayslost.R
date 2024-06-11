#' Calculate the interquartile range days lost
#'
#' Calculate the interquartile range of the days lost due to a sports-related
#' health problem (e.g. disease, injury) in a cohort.
#'
#' @inheritParams calc_ncases
#'
#' @return The interquartile range of the days lost. Either a numeric value (if
#'   overall \code{TRUE}) or a data frame indicating the interquartile range of
#'   the days lost per athlete.
#' @export
#'
#' @importFrom rlang ensym .data
#' @importFrom dplyr group_by ungroup summarise select filter
#' @importFrom stats quantile
#'
#' @examples
#' calc_iqr_dayslost(injd)
#' calc_iqr_dayslost(injd, overall = FALSE)
#' calc_iqr_dayslost(injd, by = "injury_type")
calc_iqr_dayslost <- function(injd, by = NULL, overall = TRUE) {
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
      dplyr::summarise(qt25_dayslost = stats::quantile(.data$days_lost, 0.25),
                       qt75_dayslost = stats::quantile(.data$days_lost, 0.75),
                       .groups = "keep") |>
      dplyr::ungroup() |>
      (\(x) if (is.null(by)) {
        x <- dplyr::select(x, "qt25_dayslost", "qt75_dayslost") |> t() |> c()
        names(x) <- c("qt25_dayslost", "qt75_dayslost")
        x
      } else {
        x
      })()
  } else {
    out <- injd |>
      dplyr::group_by(.data$person_id, {{ by }}) |>
      dplyr::summarise(qt25_dayslost = stats::quantile(.data$days_lost, 0.25),
                       qt75_dayslost = stats::quantile(.data$days_lost, 0.75),
                       .groups = "keep") |>
      dplyr::ungroup()
  }

  return(out)
}


