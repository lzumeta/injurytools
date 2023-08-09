#' Check the dates in injury data and exposure data
#'
#' Check that dates of injury data are within the dates of exposure data.
#' @param followup_df   Data frame created inside prepare_all() function.
#' @param data_injuries Data frame given in prepare_all() function.
#'
#' @return Same injury data or cut injury data in which the dates of injury
#'   occurrences are within the dates of each player's follow-up period.
#' @export
#'
#' @keywords internal
check_injfollowup <- function(followup_df, data_injuries) {
  data_injuries_new <- dplyr::left_join(data_injuries, followup_df,
                                        by = c("player" = "player")) |>
    droplevels()
  data_injuries_new <- data_injuries_new |>
    dplyr::filter(.data$date_injured >= .data$t0 & .data$date_injured <= .data$tf) |>
    dplyr::select(-"t0", -"tf")

  if (!identical(data_injuries, data_injuries_new)) {
    warning("Injury data has been cut to the given follow-up period (in exposure data)")
  }

  return(data_injuries_new)
}


#' constructor of injd class
#'
#' @param x a data frame object to construct it to injd class object
#' @param unit_exposure first attribute
#' @param follow_up second attribute
#' @param data_exposures third attribute
#' @param data_injuries fourth attribute
#'
#' @return a new injd object
#' @keywords internal
new_injd <- function(x = data.frame(),
                     unit_exposure = "match_minutes",
                     follow_up = data.frame(),
                     data_exposures = data.frame(),
                     data_injuries = data.frame()) {

  stopifnot(is.data.frame(x))
  unit_exposure <- match.arg(unit_exposure, c("minutes", "hours", "days",
                                              "matches_num", "matches_minutes",
                                              "activity_days", "seasons"))
  stopifnot(is.data.frame(follow_up))
  stopifnot(is.data.frame(data_exposures))
  stopifnot(is.data.frame(data_injuries))

  structure(x,
            class = "injd",
            unit_exposure      = unit_exposure,
            follow_up          = follow_up,
            data_exposures     = data_exposures,
            data_injuries      = data_injuries)

}


#' validator or injd class
#'
#' @param x an injd class object
#'
#' @return an error if x is not of injd class; otherwise x (invisibly)
#' @importFrom stats na.omit
#' @keywords internal
validate_injd <- function(x) {
  values <- unclass(x)
  unit_exposure <- attr(x, "unit_exposure")
  follow_up <- attr(x, "follow_up")
  data_exposures <- attr(x, "data_exposures")
  data_injuries <- attr(x, "data_injuries")

  ## x data frame
  exp_suffix <- exp_unit_suffix(unit_exposure)
  tstart_exp <- paste0("tstart_", exp_suffix)
  tstop_exp  <- paste0("tstop_", exp_suffix)
  need_cols <- c("player", "t0", "tf", "date_injured",
                 "date_recovered", "tstart", "tstop",
                 tstart_exp, tstop_exp, "status", "enum")
  if (!all(need_cols %in% names(values))) {
    stop("The `x` data frame has not proper column names",
         call. = FALSE)
  }
  if (!all(values$player %in% follow_up$player) | !all(x$player %in% data_exposures$player)) {
    stop("All players for whom exposure data are available should be in `x`",
         call. = FALSE)
  }
  if (any(values$date_injured > values$date_recovered, na.rm = T)) {
    stop("Injury and recovery dates are not properly recorded in `x`",
         call. = FALSE)
  }
  tstart_exp_leaded <- unlist(tapply(values[[tstart_exp]], values$player, dplyr::lead))
  if (!all(tstart_exp_leaded == values[[tstop_exp]], na.rm = T)) {
    stop("tstart and tstop are not correctly recorded in `x`",
         call. = FALSE)
  }
  if (!all(values$status %in% 0:1) | any(is.na(x$date_injured) & x$status == 1)) {
    stop("status is not correctly recorded in `x`",
         call. = FALSE)
  }

  ## unit_exposure character
  assert(checkChoice(unit_exposure, c("minutes", "hours", "days",
                                      "matches_num", "matches_minutes",
                                      "activity_days", "seasons")),
         combine = "and")

  ## follow_up data frame
  assert(is.data.frame(follow_up))
  if (!all(names(follow_up) == c("player", "t0", "tf"))) {
    stop("attr(`x`, 'follow_up') has not proper column names",
         call. = FALSE)
  }
  if (nrow(follow_up) != nlevels(values$player)) {
    stop("attr(`x`, 'follow_up') has not proper length.
         There should be one row per each player",
         call. = FALSE)
  }

  ## data_exposures data frame
  assert(is.data.frame(data_exposures))
  if (!all(c("player", "date", "time_expo") %in% names(data_exposures))) {
    stop("attr(`x`, 'data_exposures') has not the minimal key columns or these columns do not have proper names",
         call. = FALSE)
  }
  min_dates <- unlist(tapply(data_exposures$date, data_exposures$player, min))
  max_dates <- unlist(tapply(data_exposures$date, data_exposures$player, max))
  if(is.numeric(data_exposures$date)) {
    min_dates <- as.Date(date_format(paste0(min_dates, "-07-01")))
    max_dates <- as.Date(date_format(paste0(max_dates + 1 , "-06-30")))
  }
  if (any(min_dates != follow_up$t0 | max_dates != follow_up$tf)) {
    stop("attr(`x`, 'follow_up') is not properly built",
         call. = FALSE)
  }

  ## data_injuries data frame
  assert(is.data.frame(data_injuries))
  if (!all(c("player", "date_injured", "date_recovered") %in% names(data_injuries))) {
    stop("attr(`x`, 'data_injuries') has not the minimal key columns or these columns do not have proper names",
         call. = FALSE)
  }
  date_injured1  <- do.call(c, tapply(values$date_injured, values$player, function(x) stats::na.omit(x)))
  date_injured2  <- do.call(c, tapply(data_injuries$date_injured, data_injuries$player, function(x) x))
  date_recovery1 <- do.call(c, tapply(values$date_recovered, values$player, function(x) stats::na.omit(x)))
  date_recovery2 <- do.call(c, tapply(data_injuries$date_recovered, data_injuries$player, function(x) x))
  if (length(date_injured1) == length(date_injured2) & length(date_recovery1) == length(date_recovery2)) {
    if (any(date_injured1 != date_injured2 | date_recovery1 != date_recovery2)) {
      stop("attr(`x`, 'data_injuries') and `x` do not have same info on injuries",
           call. = FALSE)
    }
  } else {
    stop("attr(`x`, 'data_injuries') and `x` do not have same info on injuries",
         call. = FALSE)
  }
  x
}
