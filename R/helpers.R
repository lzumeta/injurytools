#' Check the dates in injury data and exposure data
#'
#' Check that dates of injury data are within the dates of exposure data.
#' @param followup_df   Data frame created inside prepare_all() function.
#' @param data_injuries Data frame given in prepare_all() function.
#'
#' @return Same injury data or cut injury data in which the dates of injury
#'   occurrences are within the dates of each person_id's follow-up period.
#' @export
#'
#' @keywords internal
check_injfollowup <- function(followup_df, data_injuries) {
  data_injuries_new <- dplyr::left_join(data_injuries, followup_df,
                                        by = c("person_id" = "person_id")) |>
    droplevels()
  data_injuries_new <- data_injuries_new |>
    dplyr::filter(.data$date_injured >= .data$t0 & .data$date_injured <= .data$tf) |>
    dplyr::select(-"t0", -"tf")

  if (!identical(data_injuries, data_injuries_new)) {
    warning("Injury/illness data has been cut to the given follow-up period (in exposure data)")
  }

  return(data_injuries_new)
}


#' constructor of injd class
#'
#' @param x a data frame object to construct it to injd class object
#' @param unit_exposure first attribute
#'
#' @return a new injd object
#' @keywords internal
new_injd <- function(x = data.frame(),
                     unit_exposure = "match_minutes") {

  stopifnot(is.data.frame(x))
  unit_exposure <- match.arg(unit_exposure, c("minutes", "hours", "days",
                                              "matches_num", "matches_minutes",
                                              "activity_days", "seasons"))

  structure(x,
            class = "injd",
            unit_exposure = unit_exposure)

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

  ## x data frame
  exp_suffix <- exp_unit_suffix(unit_exposure)
  tstart_exp <- paste0("tstart_", exp_suffix)
  tstop_exp  <- paste0("tstop_", exp_suffix)
  need_cols <- c("person_id", "t0", "tf", "date_injured",
                 "date_recovered", "tstart", "tstop",
                 tstart_exp, tstop_exp, "status", "enum")
  if (!all(need_cols %in% names(values))) {
    stop("The `x` data frame has not proper column names",
         call. = FALSE)
  }
  if (any(values$date_injured > values$date_recovered, na.rm = T)) {
    stop("Injury and recovery dates are not properly recorded in `x`",
         call. = FALSE)
  }
  tstart_exp_leaded <- unlist(tapply(values[[tstart_exp]], values$person_id, dplyr::lead))
  if (!all(tstart_exp_leaded == values[[tstop_exp]], na.rm = T)) {
    stop("tstart and tstop are not correctly recorded in `x`",
         call. = FALSE)
  }
  if (!all(values$status %in% 0:1) || any(is.na(x$date_injured) & x$status == 1)) {
    stop("status is not correctly recorded in `x`",
         call. = FALSE)
  }

  ## unit_exposure character
  assert(checkChoice(unit_exposure, c("minutes", "hours", "days",
                                      "matches_num", "matches_minutes",
                                      "activity_days", "seasons")),
         combine = "and")

  x
}
