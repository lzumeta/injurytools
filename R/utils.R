#' Transform injury data into a long format
#'
#' @inheritParams prepare_all
#'
#' @return The \code{data_injuries} data frame in long format in which each row
#'   corresponds to person-event.
#' @importFrom rlang .data :=
#' @importFrom dplyr select arrange mutate
#' @importFrom tidyr gather
#'
#' @keywords internal
data_injurieslong <- function(data_injuries) {
  data_injuries |>
    dplyr::select("person_id", "date_injured", "date_recovered") |>
    tidyr::gather(key = "event", value = "date", "date_injured", "date_recovered") |>
    dplyr::arrange(.data$person_id, .data$date) |>
    dplyr::mutate(event = factor(.data$event))
}


#' Proper Conversion of Date objects
#'
#' Converts \link[base:Dates]{Date} objects into a common format used for every
#' Date object throughout the package.
#'
#' To limit the scope of the changes to LC_TIME and the timezone, a temporary
#' locale modification is made using the \code{withr} package.
#'
#' @param date a \link[base:Dates]{Date} object.
#'
#' @return A "%Y-%m-%d" formatted \link[base:Dates]{Date} object with respect to
#'   a fixed locale and time zone, i.e. setting the LC_TIME component to C and
#'   timezone to UTC (the safest choice, non-geographic and Coordinated
#'   Universal Time).
#'
#' @importFrom withr with_locale
#'
#' @keywords internal
date_format <- function(date) {
  withr::with_locale(
    c("LC_TIME" = "C"),
    as.Date(date, format = "%Y-%m-%d", tz = "UTC")
  )
}

## See: https://r-pkgs.org/package-within.html#package-within-side-effects


#' Obtain suffix for time exposure unit
#'
#' @param exp_unit Character defining the unit of time exposure ("minutes" the
#'                 default).
#'
#' @return Character indicating the respective suffix for the \code{exp_unit}
#'         entered.
#'
#' @keywords internal
exp_unit_suffix <- function(exp_unit) {
  unx <- dplyr::case_when(
    exp_unit == "minutes" ~ "min",
    exp_unit == "hours" ~ "h",
    exp_unit == "matches_num" ~ "match",
    exp_unit == "matches_minutes" ~ "minPlay",
    exp_unit == "days" ~ "d",
    exp_unit == "activity_days" ~ "acd",
    exp_unit == "seasons" ~ "s"
  )
  return(unx)
}


#' Get the season
#'
#' Get the season given the date.
#'
#' @param date A vector of class \link[base:Dates]{Date} or
#'   \link[base:integer]{integer}/\link[base:numeric]{numeric}. If it is
#'   \code{integer}/\code{numeric}, it should refer to the year in which the
#'   season started (e.g. \code{date} = 2015 to refer to the 2015/2016 season)
#'
#' @return Character specifying the respective competition season given the
#'   date. The season (output) follows this pattern: "2005/2006".
#' @export
#' @importFrom lubridate year month
#' @importFrom checkmate checkDate checkNumeric
#'
#' @examples
#' date <- Sys.Date()
#' date2season(date)
date2season <- function(date) {
  assert(checkDate(date),
    checkNumeric(date),
    combine = "or"
  )

  if (inherits(date, "Date")) {
    year  <- lubridate::year(date)
    month <- lubridate::month(date)
    year1 <- ifelse(month >= 7, year, year - 1)
  } else {
    year1 <- date
  }
  season <- paste0(year1, "/", year1 + 1)

  return(season)
}

#' Get the year
#'
#' Get the year given the season.
#'
#' @param season Character/factor specifying the season. It should follow the
#'   pattern "xxxx/yyyy", e.g. "2005/2006".
#'
#' @return Given the season, it returns the year (in `numeric`) in which the
#'   season started.
#' @export
#'
#' @importFrom checkmate checkFactor checkCharacter
#' @importFrom stringr str_sub
#'
#' @examples
#' season <- "2022/2023"
#' season2year(season)
season2year <- function(season) {
  assert(checkmate::checkFactor(season),
    checkmate::checkCharacter(season),
    combine = "or"
  )
  year <- stringr::str_sub(season, start = 1, end = 4)
  return(as.numeric(year))
}

#' Cut the range of the follow-up
#'
#' Given an \code{injd} object, cut the range of the time period such that the
#' limits of the observed dates, first and last observed dates, are \code{date0}
#' and \code{datef}, respectively. It is possible to specify just one date, i.e.
#' the two dates of the range do not necessarily have to be entered. See Note
#' section.
#'
#'
#' @param injd Prepared data, an \code{injd} object.
#' @param date0 Starting date of class \link[base:Dates]{Date} or
#'   \link[base:numeric]{numeric}. If \code{numeric}, it should refer to a
#'   year (e.g. \code{date} = 2018). Optional.
#' @param datef Ending date. Same class as \code{date0}. Optional.
#'
#' @return An \code{injd} object with a shorter follow-up period.
#' @export
#'
#' @importFrom checkmate assert checkClass checkMultiClass
#' @importFrom lubridate year
#' @importFrom dplyr left_join select
#' @importFrom tidyselect starts_with
#'
#' @note Be aware that by modifying the follow-up period of the cohort, the
#'   study design is being altered. This function should not be used, unless
#'   there is no strong argument supporting it. And in that case, it should be
#'   used with caution.
#'
#' @examples
#' # Prepare data
#' \donttest{
#' df_injuries <- prepare_inj(
#'   df_injuries0   = raw_df_injuries,
#'   person_id      = "player_name",
#'   date_injured   = "from",
#'   date_recovered = "until"
#' )
#'
#' df_exposures <- prepare_exp(
#'   df_exposures0 = raw_df_exposures,
#'   person_id     = "player_name",
#'   date          = "year",
#'   time_expo     = "minutes_played"
#' )
#'
#' injd <- prepare_all(
#'   data_exposures = df_exposures,
#'   data_injuries  = df_injuries,
#'   exp_unit       = "matches_minutes"
#' )
#' }
#'
#' \donttest{
#' cut_injd(injd, date0 = 2018)
#' }
cut_injd <- function(injd, date0, datef) {
  assert(checkClass(injd, "injd"))

  data_exposures <- get_data_exposures(injd)

  if (!missing(date0)) {
    assert(checkMultiClass(date0, c("Date", "numeric", "integer")))
    if (is.numeric(date0)) data_exposures$date <- season2year(date2season(data_exposures$date))
  }
  if (!missing(datef)) {
    assert(checkMultiClass(datef, c("Date", "numeric", "integer")))
    if (is.numeric(datef)) data_exposures$date <- season2year(date2season(data_exposures$date))
  }
  if ((!missing(date0) && !missing(datef))) {
    if (date0 >= datef) {
      stop("date0 must be < datef")
    }
  }

  data_injuries <- get_data_injuries(injd) |>
    dplyr::left_join(injd, by = c("person_id", "date_injured", "date_recovered")) |>
    dplyr::select(-c("t0", "tf", tidyselect::starts_with("tstart"), tidyselect::starts_with("tstop"),
                     "status", "enum", "days_lost"))
  exp_unit <- attr(injd, "unit_exposure")

  if (!missing(date0)) {
    data_exposures <- data_exposures |>
      dplyr::filter(.data$date >= date0)
    data_injuries <- data_injuries |>
      dplyr::filter(.data$date_injured >= date0 |
        lubridate::year(.data$date_injured) >= date0)
  }

  if (!missing(datef)) {
    data_exposures <- data_exposures |>
      dplyr::filter(.data$date <= datef)
    data_injuries <- data_injuries |>
      dplyr::filter(.data$date_injured <= datef |
        lubridate::year(.data$date_injured) >= datef)
  }

  if (!nrow(data_exposures)) stop("Empty injd object")

  injd_new <- prepare_all(
    data_exposures,
    data_injuries,
    exp_unit
  )

  return(injd_new)
}

#' Extract follow-up data frame
#'
#' Extract follow-up data frame from the injd object.
#'
#' @inheritParams calc_ncases
#'
#' @return The follow-up data frame containing the necessary columns:
#'   "person_id", "t0" and "tf".
#'
#' @export
#' @importFrom dplyr summarise first last
#'
#' @examples
#' get_data_followup(injd)
get_data_followup <- function(injd) {
  data.frame(injd) |>
    dplyr::summarise(t0 = dplyr::first(.data$tstart),
                     tf = dplyr::last(.data$tstop),
                     .by = "person_id")
}


#' Extract injury/illness data frame
#'
#' Extract injury/illness data frame from the injd object.
#'
#' @inheritParams calc_ncases
#'
#' @return The injury/illness data frame containing the necessary columns:
#'   "person_id", "date_injured" and "date_recovered".
#'
#' @importFrom dplyr filter select
#' @export
#'
#' @examples
#' get_data_injuries(injd)
get_data_injuries <- function(injd) {
  data.frame(injd) |>
    dplyr::filter(.data$status == 1) |>
    droplevels() |>
    dplyr::select("person_id", "date_injured", "date_recovered")
}


#' Extract exposures data frame
#'
#' Extract exposures data frame from the injd object.
#'
#' @inheritParams calc_ncases
#'
#' @return The exposure data frame containing the necessary columns:
#'   "person_id", "date" and "time_expo".
#' @export
#'
#' @importFrom dplyr select filter mutate
#' @importFrom tidyselect starts_with
#'
#' @examples
#' get_data_exposures(injd)
get_data_exposures <- function(injd) {
  unx <- exp_unit_suffix(attr(injd, "unit_exposure"))
  data.frame(injd) |>
    dplyr::select("person_id", date = "tstop",
                  tstart_unx = tidyselect::matches(paste0("tstart_", unx)),
           tstop_unx = tidyselect::matches(paste0("tstop_", unx))) |>
    dplyr::mutate(time_expo = .data$tstop_unx - .data$tstart_unx) |>
    dplyr::filter(.data$time_expo != 0) |>
    dplyr::select("person_id", "date", "time_expo")
}
