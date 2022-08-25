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
#' @inheritParams date_format
#'
#' @return Character specifying the respective competition season given the
#'   date.
#' @export
#' @importFrom lubridate year month
#' @importFrom checkmate checkDate
#'
#' @examples
#' date <- Sys.Date()
#' date2season(date)
date2season <- function(date) {
  assert(checkDate(date))

  year <- lubridate::year(date)
  month <- lubridate::month(date)
  year1 <- ifelse(month >= 7, year, year - 1)
  season <- paste0(year1, "/", year1 + 1)

  return(season)
}


#' Cut the range of the follow-up
#'
#' Given an \code{injd} object cut the range of the time period such that the
#' limits of the observed dates, first and last observed dates, are \code{date0}
#' and \code{datef}, respectively. It is possible to specify just one date, i.e.
#' the two dates of the range do not necessarily have to be entered. See Note
#' section.
#'
#'
#' @param injd Prepared data, an \code{injd} object.
#' @param date0 Starting date, of class \link[base:Dates]{Date} or
#'   \link[base:numeric]{numeric} and if \code{numeric} it should refer to a
#'   year (e.g. \code{date} = 2018). Optional.
#' @param datef Ending date. Same class as \code{date0}. Optional.
#'
#' @return An \code{injd} object with a shorter follow-up period.
#' @export
#'
#' @importFrom checkmate assert checkClass checkMultiClass
#' @importFrom lubridate year
#'
#' @note Be aware that by modifying the follow-up period of the cohort, the
#'   study design is being altered. This function should not be used, unless
#'   there is no strong argument supporting it. And in that case, it should be
#'   used with caution.
#'
#' @examples
#' # Prepare data
#' # df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
#' #                            player         = "player_name",
#' #                            date_injured   = "from",
#' #                            date_recovered = "until")
#' #
#' # df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
#' #                             player        = "player_name",
#' #                             date          = "year",
#' #                             time_expo     = "minutes_played")
#' #
#' # injd <- prepare_all(data_exposures = df_exposures,
#' #                     data_injuries  = df_injuries,
#' #                     exp_unit = "matches_minutes")
#'
#'
#' # Cut the follow-up period of the object
#' # (please, use it carefully)
#' cut_injd(injd, date0 = 2018)
cut_injd <- function(injd, date0, datef) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  data_exposures <- attr(injd, "data_exposures")

  if (!missing(date0)) {
    assert(checkMultiClass(date0, c("Date", "numeric", "integer")))
    if (class(date0) != class(data_exposures$date)) {
      stop("date0 and data_exposures[['date']] must be of the same class")
    }
  }
  if (!missing(datef)) {
    assert(checkMultiClass(datef, c("Date", "numeric", "integer")))
    if (class(datef) != class(data_exposures$date)) {
      stop("datef and data_exposures[['date']] must be of the same class")
    }
  }
  if ((!missing(date0) & !missing(datef))) {
    if (class(date0) != class(datef)) {
      stop("date0 and datef must be of the same class")
    }
    if (date0 >= datef) {
      stop("date0 must be < datef")
    }
  }

  data_injuries <- attr(injd, "data_injuries")
  exp_unit <- attr(injd, "unit_exposure")

  if (!missing(date0)) {
    data_exposures <- data_exposures %>%
      dplyr::filter(.data$date >= date0)
    data_injuries <- data_injuries %>%
      dplyr::filter(.data$date_injured >= date0 |
                      lubridate::year(.data$date_injured) >= date0)
  }

  if (!missing(datef)) {
    data_exposures <- data_exposures %>%
      dplyr::filter(.data$date <= datef)
    data_injuries <- data_injuries %>%
      dplyr::filter(.data$date_injured <= datef |
                      lubridate::year(.data$date_injured) >= date0)
  }

 if (!nrow(data_exposures)) stop("Empty injd object")

 injd_new <- prepare_all(data_exposures,
                         data_injuries,
                         exp_unit)

  return(injd_new)
}

## injfilter (?) I consider that it's better to filter exposure and injury data