#' Check that dates in injury data and exposure data
#'
#' Check that dates of injury data are within the dates of exposure data.
#' @param followup_df   Data frame created inside prepare_all() function.
#' @param data_injuries Data frame given in prepare_all() function.
#'
#' @return Same injury data or cut in which the dates of injury occurrences
#'   are within the dates of each player's follow-up period.
#' @export
#'
#' @keywords internal
check_injfollowup <- function(followup_df, data_injuries) {
  data_injuries_new <- dplyr::left_join(data_injuries, followup_df,
                             by = c("player" = "player")) %>%
    droplevels()
  data_injuries_new <- data_injuries_new %>%
    dplyr::filter(.data$date_injured >= .data$t0 & .data$date_injured <= .data$tf) %>%
    dplyr::select(-"t0", -"tf")

  if (!identical(data_injuries, data_injuries_new)) {
    warning("Injury data has been cut to the given follow-up period (in exposure data)")
  }

  return(data_injuries_new)
}
