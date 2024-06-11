#' Prepare data in a standardized format
#'
#' These are the data preprocessing functions provided by the \code{injurytools}
#' package, which involve:
#' \enumerate{
#'   \item setting \strong{exposure} and \strong{injury/illness data} in a standardized
#'   format and
#'   \item integrating both sources of data into an adequate data structure.
#' }
#' \code{prepare_inj()} and \code{prepare_exp()} set standardized names and
#' proper classes to the (key) columns in injury/illness and exposure data,
#' respectively. \code{prepare_all()} integrates both, standardized injury and
#' exposure data sets, and convert them into an \code{injd} \strong{S3} object
#' that has an adequate structure for further statistical analyses. See the
#' \href{https://lzumeta.github.io/injurytools/articles/prepare-injury-data.html}{Prepare
#' Sports Injury Data} vignette for details.
#'
#' @name prepare_data
NULL
#> NULL
#'
#' @rdname prepare_data
#' @param df_injuries0 A data frame containing injury or illness information,
#'   with columns referring to the athlete name/id, date of injury/illness and
#'   date of recovery (as minimal data).
#' @param person_id Character referring to the column name storing sportsperson
#'   (player, athlete) identification information.
#' @param date_injured Character referring to the column name where the
#'   information about the date of injury or illness is stored.
#' @param date_recovered Character referring to the column name where the
#'   information about the date of recovery is stored.
#'
#' @return \code{prepare_inj()} returns a data frame in which the \strong{key
#'   columns} in injury/illness data are standardized and have a proper format.
#'
#' @importFrom checkmate assert
#' @importFrom checkmate checkClass checkMultiClass
#' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
#'                            person_id      = "player_name",
#'                            date_injured   = "from",
#'                            date_recovered = "until")
#'
prepare_inj <- function(df_injuries0,
                        person_id = "person_id",
                        date_injured = "date_injured",
                        date_recovered = "date_recovered") {
  ## check inputs
  assert(
    checkClass(df_injuries0, "data.frame")
  )
  assert(
    checkMultiClass(df_injuries0[[person_id]], c("factor", "character")),
    checkClass(df_injuries0[[date_injured]], "Date"),
    checkClass(df_injuries0[[date_recovered]], "Date"),
    combine = "and")
  if (any(df_injuries0[[date_injured]] > df_injuries0[[date_recovered]])) stop("date_recovered should be >= date_injured")

  ## rename
  data_injuries <- dplyr::rename(df_injuries0,
                                 "person_id"      = tidyselect::all_of(person_id),
                                 "date_injured"   = tidyselect::all_of(date_injured),
                                 "date_recovered" = tidyselect::all_of(date_recovered)) |>
    dplyr::mutate(person_id = factor(person_id),
                  date_injured = as.Date(date_format(date_injured)),
                  date_recovered = as.Date(date_format(date_recovered)))
  return(data_injuries)
}



#' @rdname prepare_data
#' @param df_exposures0 A data frame containing exposure information, with
#'   columns referring to the sportsperson's name/id, date of exposure and the
#'   total time of exposure of the corresponding data entry (as minimal data).
#' @param person_id Character referring to the column name storing sportsperson
#'   (player, athlete) identification information.
#' @param date Character referring to the column name where the exposure date
#'   information is stored. Besides, the column must be of class
#'   \link[base:Dates]{Date} or
#'   \link[base:integer]{integer}/\link[base:numeric]{numeric}. If it is
#'   \code{integer}/\code{numeric}, it should refer to the year in which the
#'   season started (e.g. \code{date} = 2015 to refer to the 2015/2016 season).
#' @param time_expo Character referring to the column name where the information
#'   about the time of exposure in that corresponding date is stored.
#'
#' @return \code{prepare_exp()} returns a data frame in which the \strong{key
#'   columns} in exposure data are standardized and have a proper format.
#'
#' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
#'                             person_id     = "player_name",
#'                             date          = "year",
#'                             time_expo     = "minutes_played")
#'
prepare_exp <- function(df_exposures0,
                        person_id = "person_id",
                        date = "date",
                        time_expo = "time_expo") {
  ## check inputs
  assert(
    checkClass(df_exposures0, "data.frame"),
    checkMultiClass(df_exposures0[[person_id]], c("factor", "character")),
    checkMultiClass(df_exposures0[[date]], c("Date", "numeric", "integer")),
    checkMultiClass(df_exposures0[[time_expo]], c("numeric", "integer")),
    combine = "and"
  )
  if (class(df_exposures0[[date]]) %in% c("integer", "numeric")) {
    if (any(nchar(as.character(df_exposures0[[date]])) != 4)) {
      stop("If the 'date' column is numeric, it must refer to the year")
    }
  }

  ## rename
  data_exposures <- dplyr::rename(df_exposures0,
                                  "person_id" = tidyselect::all_of(person_id),
                                  "date"      = tidyselect::all_of(date),
                                  "time_expo" = tidyselect::all_of(time_expo)) |>
    dplyr::mutate(person_id = factor(person_id),
                  date = as.character(date))
  if (nchar(data_exposures$date[[1]]) == 4) {
    data_exposures$date <- as.numeric(data_exposures$date)
  } else {
    data_exposures$date <- as.Date(date_format(data_exposures$date))
  }

  return(data_exposures)
}


#' Build follow-up data frame
#'
#' @inheritParams prepare_all
#'
#' @return A data frame in which each row corresponds to a sportsperson and
#'   his/her first date (\code{t0}) and last date (\code{tf}) observed.
#' @importFrom rlang .data
#' @importFrom lubridate year
#' @keywords internal
data_followup <- function(data_exposures) {
  ## check inputs
  assert(checkMultiClass(data_exposures[["date"]], c("Date", "numeric", "integer")))
  if (class(data_exposures$date) %in% c("integer", "numeric")) {
    if (any(nchar(as.character(data_exposures$date)) != 4)) {
      stop("If the 'date' column is numeric, it must refer to the year")
    }
  }

  followup_df <- data_exposures |>
    dplyr::group_by(.data$person_id) |>
    dplyr::summarise(t0 = min(.data$date),
                     tf = max(.data$date)) |>
    dplyr::ungroup()

  ## if seasons(date of class numeric).. t0 beginning of the (pre)season (July);
  ## tf the end of the season (June first)
  if (is.numeric(data_exposures$date)) {
    followup_df$t0 <- as.Date(date_format(paste0(followup_df$t0, "-07-01")))
    followup_df$tf <- as.Date(date_format(paste0(followup_df$tf + 1, "-06-30")))
  }

  return(followup_df)
}


#' @rdname prepare_data
#' @param data_exposures Exposure data frame with standardized column names, in
#'   the same fashion that \code{prepare_exp()} returns.
#' @param data_injuries Injury data frame with standardized column names, in the
#'   same fashion that \code{prepare_inj()} returns.
#' @param exp_unit Character defining the unit of exposure time ("minutes" the
#'   default).
#'
#' @return \code{prepare_all()} returns the \code{injd} \strong{S3} object that
#'   contains all the necessary information and a proper data structure to
#'   perform further statistical analyses (e.g. calculate injury summary
#'   statistics, visualize injury data).
#'
#'   * If \code{exp_unit} is "minutes" (the default), the columns
#'   \code{tstart_min} and \code{tstop_min} are created which specify the time
#'   to event (injury) values, the starting and stopping time of the interval,
#'   respectively. That is the training time in minutes, that the sportsperson has
#'   been at risk, until an injury/illness (or censorship) has occurred. For other
#'   choices, \code{tstart_x} and \code{tstop_x} are also created according to
#'   the \code{exp_unit} indicated (\code{x}, one of: \code{min}, \code{h},
#'   \code{match}, \code{minPlay}, \code{d}, \code{acd} or \code{s}). These
#'   columns will be useful for survival analysis routines. See Note section.
#'
#'   * It also creates \code{days_lost} column based on the difference between
#'   \code{date_recovered} and \code{date_injured} in days. And if it does exist (in
#'   the raw data) it overrides.
#'
#' @note
#'  Depending on the unit of exposure, \code{tstart_x} and \code{tstop_x}
#'  columns might have same values (e.g. if `exp_unit` = "matches_num" and the
#'  player has not played any match between the corresponding period of time).
#'  Please be aware of this before performing any survival analysis related
#'  task.
#'
#' @importFrom rlang .data := !! sym
#' @importFrom dplyr n row_number filter left_join arrange group_by ungroup
#'   slice distinct mutate lag lead select across if_else first last
#'   pull do bind_rows rowwise
#' @importFrom checkmate checkChoice
#' @importFrom tidyselect all_of everything
#' @export
#'
#' @examples
#' \donttest{
#' injd <- prepare_all(data_exposures = df_exposures,
#'                     data_injuries  = df_injuries,
#'                     exp_unit = "matches_minutes")
#' head(injd)
#' class(injd)
#' str(injd, 1)
#' }
prepare_all <- function(data_exposures,
                        data_injuries,
                        exp_unit = c("minutes", "hours", "days",
                                     "matches_num", "matches_minutes",
                                     "activity_days", "seasons")) {
  # Check inputs
  assert(checkChoice(exp_unit, c("minutes", "hours", "days",
                                 "matches_num", "matches_minutes",
                                 "activity_days", "seasons")),
         combine = "and")

  followup_df   <- data_followup(data_exposures)
  data_injuries <- check_injfollowup(followup_df, data_injuries)

  injd <- followup_df |>
    dplyr::left_join(data_injuries, by = "person_id", multiple = "all") |>
    dplyr::arrange(.data$person_id, .data$t0, .data$date_injured)

  # Create two variables of date type: tstart and tstop
  injd <- injd |>
    dplyr::group_by(.data$person_id) |>
    dplyr::do({
      last_row <- dplyr::slice_tail(., n = 1) |>
        dplyr::mutate(dplyr::across(-c("person_id", "t0", "tf"), ~ NA))
      dplyr::bind_rows(., last_row)
    }) |>
    dplyr::distinct() |>
    dplyr::mutate(tstart = dplyr::lag(.data$date_recovered, default = dplyr::first(.data$t0)),
                  tstop  = dplyr::if_else(dplyr::row_number() < dplyr::n(), .data$date_injured, dplyr::last(.data$tf))) |>
    dplyr::select("person_id", "t0", "tf", "date_injured", "date_recovered",
                  "tstart", "tstop", tidyselect::everything()) |>
    dplyr::ungroup()

  # Fill tstop for non-injured players and delete invalid rows (i.e. observation
  # whose recovery date is later than the end of her follow-up)
  injd <- injd |>
    dplyr::group_by(.data$person_id) |>
    dplyr::mutate(tstop = dplyr::if_else(dplyr::row_number() == 1 & is.na(.data$tstop),
                                         .data$tf[1],
                                         .data$tstop)) |>
    dplyr::filter(.data$tstart <= .data$tstop) |>
    droplevels() |>
    dplyr::ungroup()

  # Obtain exposure units
  # 1) create unx suffix, i.e. tstart_unx and tstop_unx
  unx <- exp_unit_suffix(exp_unit)
  tstart_unx <- paste0("tstart_", unx)
  tstop_unx  <- paste0("tstop_", unx)

  injd <- injd |>
    mutate(
      !!tstart_unx := 0,
      !!tstop_unx  := 0
    )

  # Create date_aux
  ## populate tstop_unx column
  if (is.numeric(data_exposures$date)) {
    data_exposures <- data_exposures |>
      mutate(date_aux = as.Date(paste0(date, "-07-01")),
             season2  = date2season(.data$date_aux)) |>
      dplyr::select("person_id", "season2", "time_expo")
    # injd <- injd |>
    #   # mutate(season2 = dplyr::if_else(lubridate::month(.data$tstart) <=6 & (lubridate::month(.data$tstop) >= 7 | lubridate::year(.data$tstart) < lubridate::year(.data$tstop)),
    #   #                                 date2season(.data$tstop),
    #   #                                 date2season(.data$tstart))) |>
    #   mutate(tstart = date2season(.data$tstart)) |>
    #   left_join(data_exposures, by = c("person_id", "season2")) |>
    #   mutate(!!tstop_unx:= dplyr::if_else(dplyr::row_number() == 1, time_expo, 0),
    #          .by = c("person_id", "season2")) |>
    #   dplyr::select(-c("time_expo", "season2"))

    exposure_summary <- data_exposures |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarize(date_expo = list(.data$season2),
                       time_expo = list(.data$time_expo),
                       .groups = 'drop')

    injd <- injd |>
      dplyr::mutate(tstart2 = date2season(.data$tstart),
                    tstop2  = date2season(.data$tstop)) |>
      dplyr::mutate(tstart2_lag = dplyr::lag(.data$tstart2, default = "0"),
                    tstart2 = ifelse(.data$tstart2 == .data$tstart2_lag & date2season(.data$tstart) != date2season(.data$tstop),
                         date2season(as.Date(paste0(season2year(date2season(.data$tstart))+1, "-07-01"))),
                         .data$tstart2),
                    tstop2_lead = dplyr::lead(.data$tstop2, default = "0"),
                    tstop2 = ifelse(.data$tstop2 == .data$tstop2_lead & .data$tstart2 != .data$tstop2_lead,
                                    date2season(as.Date(paste0(season2year(date2season(.data$tstop))-1, "-07-01"))),
                                    .data$tstop2),
                    .by = "person_id") |>
      dplyr::left_join(exposure_summary, by = "person_id") |>
      dplyr::rowwise() |>
      # dplyr::mutate(!!tstop_unx := purrr::map2_dbl(.data$date_expo, .data$time_expo, ~ sum(.y[.x >= .data$tstart & .x < .data$tstop]))) |>
      dplyr::mutate(!!tstop_unx := sum(.data$time_expo[.data$date_expo >= .data$tstart2 & .data$date_expo <= .data$tstop2])) |>
      dplyr::ungroup() |>
      mutate(!!tstop_unx:= dplyr::if_else(dplyr::row_number() == 1 , !!sym(tstop_unx), 0),
             .by = c("person_id", "tstart2")) |>
      dplyr::select(-c("date_expo", "time_expo", "tstart2", "tstart2_lag", "tstop2", "tstop2_lead"))
  } else {
    data_exposures <- data_exposures |>
      mutate(date_aux = date)

    exposure_summary <- data_exposures |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarize(date_expo = list(.data$date_aux),
                       time_expo = list(.data$time_expo),
                       .groups = 'drop')

    injd <- injd |>
      dplyr::left_join(exposure_summary, by = "person_id") |>
      dplyr::rowwise() |>
      # dplyr::mutate(!!tstop_unx := purrr::map2_dbl(.data$date_expo, .data$time_expo, ~ sum(.y[.x >= .data$tstart & .x < .data$tstop]))) |>
      dplyr::mutate(!!tstop_unx := sum(.data$time_expo[.data$date_expo >= .data$tstart & .data$date_expo < .data$tstop])) |>
      dplyr::ungroup() |>
      dplyr::select(-c("date_expo", "time_expo"))
  }

  ## Add enum, status, and days_lost columns; edit tstop_unx and tstart_unx
  injd <- injd |>
    dplyr::group_by(.data$person_id) |>
    dplyr::mutate(
      !!tstop_unx  := cumsum(.data[[tstop_unx]]),
      !!tstart_unx := lag(.data[[tstop_unx]], default = 0),
      enum   = dplyr::lag(cumsum(!is.na(.data$date_injured)), default = 0) + 1,
      status = dplyr::if_else(is.na(.data$date_injured), 0, 1),
      days_lost = dplyr::if_else(is.na(.data$date_injured),
                                 0,
                                 as.numeric(difftime(.data$date_recovered, .data$date_injured, units = "days")))
    ) |>
    dplyr::ungroup() |>
    dplyr::select("person_id", "t0", "tf",
                  "date_injured", "date_recovered", "tstart", "tstop",
                  tidyselect::all_of(tstart_unx), tidyselect::all_of(tstop_unx),
                  "status", "enum", "days_lost", tidyselect::everything())

  # Set class and attributes for `injd` object
  class(injd) <- c("injd", class(injd))
  attr(injd, "unit_exposure")  <- exp_unit

  return(injd)
}

#' Check if an object is of class \code{injd}
#'
#' Check if an object \code{x} is of class \code{injd}.
#'
#' @param x any R object.
#' @return A logical value: \code{TRUE} if \code{x} inherits from \code{injd}
#'   class, \code{FALSE} otherwise.
#' @export
is_injd <- function(x) inherits(x, "injd")


