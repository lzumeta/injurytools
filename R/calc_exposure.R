#' Calculate the exposure time
#'
#' Calculate the time of exposure that each athlete, or the entire cohort of
#' athletes, has been at risk for a sport-related health problem.
#'
#' @inheritParams calc_ncases
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return The total exposure time. Either a numeric value (if overall
#'   \code{TRUE}) or a data frame indicating the total exposure time for each
#'   athlete.
#' @export
#' @importFrom rlang ensym !! as_string .data
#' @importFrom dplyr group_by ungroup summarise select arrange
#' @importFrom tibble tibble
#'
#' @examples
#' calc_exposure(injd)
#' calc_exposure(injd, overall = FALSE)
#' calc_exposure(injd, by = "injury_type")
calc_exposure <- function(injd, by = NULL, overall = TRUE,
                          quiet = FALSE) {

  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))
  assert(checkLogical(overall))

  if (!is.null(by)) {
    by <- rlang::ensym(by)
  }

  df_exposures <- get_data_exposures(injd)

  ## if the by variable is a player characteristic (rather than an
  ## injury-related variable) then compute that characteristic-specific
  ## exposure sum. That will be determined whether that "by" variable
  ## contains NAs (then, should be injury related) or not (then,
  ## should be a player characteristic
  if (!is.null(by)) aux <- sum(is.na(injd[[by]])) else aux <- 9999
  if (!is.null(by) & aux == 0) {
    df_aux <- injd |> dplyr::select("person_id", by) |> unique()
    df_exposures <- left_join(df_exposures, df_aux, by = "person_id")
    df_exposures_summary <- df_exposures |>
      dplyr::group_by(.data$person_id, .data[[by]]) |>
      dplyr::summarise(totalexpo = sum(.data$time_expo)) |>
      dplyr::ungroup()
  } else {
    df_exposures_summary <- df_exposures |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(totalexpo = sum(.data$time_expo)) |>
      dplyr::ungroup()
  }


  ## if total expo = 0, omit those variables and throw a message
  if (any(df_exposures_summary$totalexpo == 0)) {
    if (!quiet) {
      warning(paste0("\n* There are athletes with no exposure..\n",
                     "  We assume that they have not been exposed to a health problem and thus,",
                     " they are omitted from this summary\n"))
    }
    quit_players <- df_exposures_summary |>
      dplyr::filter(.data$totalexpo == 0) |>
      dplyr::select("person_id") |>
      (\(x) x[[1]])() |>
      as.character()
    injd <- injd |>
      dplyr::filter(.data$person_id != quit_players) |>
      droplevels()
  }

  if (overall) {
    totalexpo <- sum(df_exposures_summary$totalexpo)
    if (!is.null(by)) {
      if (aux == 0) {
        totalexpo <- summarise(df_exposures_summary,
                               totalexpo = sum(totalexpo),
                               .by = by)
        out <- totalexpo
      } else {
        out <- tibble::tibble(!!by := levels(factor(injd[[rlang::as_string(by)]])),
                              totalexpo = totalexpo)
      }
    } else {
      out <- c(totalexpo = totalexpo)
    }
  } else {
    if(!is.null(by)) {
      out <- cbind(tibble::tibble(
        !!by := rep(levels(factor(injd[[rlang::as_string(by)]])), nrow(df_exposures_summary))
      ),
      df_exposures_summary) |>
        dplyr::arrange(.data$person_id, .data[[rlang::as_string(by)]]) |>
        dplyr::select("person_id", rlang::as_string(by), "totalexpo")
    } else {
      out <- df_exposures_summary
    }
  }

  return(out)
}
