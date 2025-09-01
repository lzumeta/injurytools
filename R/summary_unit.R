#' Transform calc_summary() output according to the unit of exposure
#'
#' @param unit Character that indicates the unit of exposure of the
#'   sports-related health problem data.
#' @param summary_data \code{summary_data} data frame to be transformed.
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return The same calc_summary() data frame with
#'   'incidence' and 'cburden' values transformed according to \code{unit},
#'   named \code{summary_data}.
#'
#' @keywords internal
summary_unit <- function(unit, summary_data, quiet) {
  if (unit %in% c("minutes", "hours")) {
    if (!quiet) {
      warning(paste0("\n Exposure time unit is ", unit,
                     "\n Case incidence and case burden are calculated per 1000h of athlete-exposure\n\n"))
    }
    if (unit == "minutes") {
      summary_data <- summary_data |>
        dplyr::mutate(dplyr::across(tidyselect::any_of(tidyselect::starts_with(c("incidence", "burden"))), ~ .x * 60 * 1000))
    } else {
      summary_data <- summary_data |>
        dplyr::mutate(dplyr::across(tidyselect::any_of(tidyselect::starts_with(c("incidence", "burden"))), ~ .x * 1000))
    }
  } else if (unit == "matches_minutes") {
    if (!quiet) {
      warning(paste0("\n Exposure time unit is ", unit,
                     "\n Case incidence and case burden are calculated per 100 athlete-matches of exposure (i.e. 90 minutes times 100)\n\n"))
    }
    summary_data <- summary_data |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(tidyselect::starts_with(c("incidence", "burden"))), ~ .x * 90 * 100))
  } else if (unit %in% c("matches_num", "days", "activity_days")) {
    if (!quiet) {
      warning(paste0("\n Exposure time unit is ", unit,
                     "\n Case incidence and case burden are calculated per 100 of ", unit, " of exposure\n\n"))
    }
    summary_data <- summary_data |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(tidyselect::starts_with(c("incidence", "burden"))), ~ .x * 100))
    unit <- dplyr::case_when(unit == "matches_num" ~ "matches",
                             unit == "days" ~ "days",
                             unit == "activity_days" ~ "activity days")
  }

  return(summary_data)
}



#' Label the time unit
#'
#' An auxiliary function to return the label of the exposure time unit used.
#'
#' @param unit Character that indicates the unit of exposure of the
#'   sports-related health problem data.
#'
#' @return A character vector that expresses the
#'   unit used for the rates, i.e. for the athlete's time at risk, named
#'   \code{unit_timerisk}.
#'
#' @keywords internal
label_unit <- function(unit) {
  unit_aux <- dplyr::case_when(unit == "matches_num" ~ "matches",
                               unit == "days" ~ "days",
                               unit == "activity_days" ~ "activity days",
                               .default = unit)
  dplyr::case_when(unit_aux %in% c("minutes", "hours") ~ "1000h athlete-exposure",
                   unit_aux == "matches_minutes" ~ "100 athlete-match",
                   unit_aux %in% c("matches", "days", "activity days") ~ paste0("100 athlete-", unit_aux),
                   unit_aux == "seasons"~ "athlete-seasons"
                   )
}
