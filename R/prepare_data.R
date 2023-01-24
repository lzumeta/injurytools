#' Prepare data in a standardized format
#'
#' These are the data preprocessing functions provided by the \code{injurytools}
#' package, which involve:
#' \enumerate{
#'   \item setting \strong{exposure} and \strong{injury data} in a standardized
#'   format and
#'   \item integrating both sources of data into an adequate data structure.
#' }
#' \code{prepare_inj()} and \code{prepare_exp()} set standardized names and
#' proper classes to the (key) columns in injury and exposure data,
#' respectively. \code{prepare_all()} integrates both, standardized injury and
#' exposure data sets, and convert them into an \code{injd} \strong{S3} object
#' that has an adequate structure for further statistical analyses.
#' See the \href{https://lzumeta.github.io/injurytools/articles/prepare-injury-data.html}{Prepare Sports Injury Data}
#' vignette for details.
#'
#' @name prepare_data
NULL
#> NULL
#'
#' @rdname prepare_data
#' @param df_injuries0 A data frame containing injury information, with columns
#'   referring to the player name/id, date of injury and date of recovery (as
#'   minimal data).
#' @param player Character referring to the column name where player information
#'   is stored.
#' @param date_injured Character referring to the column name where the
#'   information about the date of injury is stored.
#' @param date_recovered Character referring to the column name where the
#'   information about the date of recovery is stored.
#'
#' @return \code{prepare_inj()} returns a data frame in which the \strong{key
#'   columns} in injury data are standardized and have a proper format.
#'
#' @importFrom checkmate assert
#' @importFrom checkmate checkClass checkMultiClass
#' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
#'                            player         = "player_name",
#'                            date_injured   = "from",
#'                            date_recovered = "until")
#'
prepare_inj <- function(df_injuries0,
                        player = "player",
                        date_injured = "date_injured",
                        date_recovered = "date_recovered") {
  ## check inputs
  assert(
    checkClass(df_injuries0, "data.frame")
  )
  assert(
    checkMultiClass(df_injuries0[[player]], c("factor", "character")),
    checkClass(df_injuries0[[date_injured]], "Date"),
    checkClass(df_injuries0[[date_recovered]], "Date"),
    combine = "and")
  if (any(df_injuries0[[date_injured]] > df_injuries0[[date_recovered]])) stop("date_recovered should be >= date_injured")

  ## rename
  data_injuries <- dplyr::rename(df_injuries0,
                                 "player"         = tidyselect::all_of(player),
                                 "date_injured"   = tidyselect::all_of(date_injured),
                                 "date_recovered" = tidyselect::all_of(date_recovered)) %>%
    dplyr::mutate(player = factor(player),
                  date_injured = as.Date(date_format(date_injured)),
                  date_recovered = as.Date(date_format(date_recovered)))
  return(data_injuries)
}



#' @rdname prepare_data
#' @param df_exposures0 A data frame containing exposure information, with
#'   columns referring to the player name/id, date of exposure and the total
#'   time of exposure of the corresponding data entry (as minimal data).
#' @param player Character referring to the column name where player information
#'   is stored.
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
#'                             player        = "player_name",
#'                             date          = "year",
#'                             time_expo     = "minutes_played")
#'
prepare_exp <- function(df_exposures0,
                        player = "player",
                        date = "date",
                        time_expo = "time_expo") {
  ## check inputs
  assert(
    checkClass(df_exposures0, "data.frame"),
    checkMultiClass(df_exposures0[[player]], c("factor", "character")),
    checkMultiClass(df_exposures0[[date]], c("Date", "numeric", "integer")),
    checkClass(df_exposures0[[time_expo]], "numeric"),
    combine = "and"
  )
  if (class(df_exposures0[[date]]) %in% c("integer", "numeric")) {
    if (any(nchar(as.character(df_exposures0[[date]])) != 4)) {
      stop("If the 'date' column is numeric, it must refer to the year")
    }
  }

  ## rename
  data_exposures <- dplyr::rename(df_exposures0,
                                  "player"    = tidyselect::all_of(player),
                                  "date"      = tidyselect::all_of(date),
                                  "time_expo" = tidyselect::all_of(time_expo)) %>%
    dplyr::mutate(player = factor(player),
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
#' @return A data frame in which each row corresponds to a player and his/her
#'         first date (\code{t0}) and last date (\code{tf}) observed.
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

  followup_df <- data_exposures %>%
    dplyr::group_by(.data$player) %>%
    dplyr::summarise(t0 = min(.data$date),
                     tf = max(.data$date)) %>%
    dplyr::ungroup()

  ## if seasons(date of class numeric).. t0 beginning of the (pre)season (July);
  ## tf the end of the season (June first)
  if (is.numeric(data_exposures$date)) {
    followup_df$t0 <- as.Date(date_format(paste0(followup_df$t0, "-07-01")))
    followup_df$tf <- as.Date(date_format(paste0(followup_df$tf + 1 , "-06-30")))
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
#'   respectively. That is the training time in minutes, that the player has
#'   been at risk, until an injury (or censorship) has occurred. For other
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
#' @importFrom rlang .data := !!
#' @importFrom dplyr n row_number filter left_join arrange group_by ungroup
#'   slice mutate_at distinct mutate lag select
#' @importFrom purrr pmap_dbl
#' @importFrom checkmate checkChoice
#' @importFrom tidyselect all_of everything
#' @export
#'
#' @examples
#' \dontrun{
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

  ## check inputs
  assert(checkChoice(exp_unit, c("minutes", "hours", "days",
                                 "matches_num", "matches_minutes",
                                 "activity_days", "seasons")),
         combine = "and")

  followup_df <- data_followup(data_exposures)

  data_injuries <- check_injfollowup(followup_df, data_injuries)

  injd <- followup_df %>%
    dplyr::left_join(data_injuries, by = c("player" = "player")) %>%
    dplyr::arrange(.data$player, .data$t0, .data$date_injured)

  ## Create two variables of date type: tstart and tstop
  injd <- injd %>%
    dplyr::group_by(.data$player) %>%
    dplyr::slice(1:n(), n()) %>%  ## copy last row
    dplyr::mutate_at(vars(-"player", -"t0", -"tf"), ~replace(.x, row_number() == n(), NA)) %>%
    ## edit this last row with slice and add NAs at every variable except for (Jug, t0, tf)
    ## This step is done in order to arrive until the last follow-up date
    ## (base::replace() in order to keep the columns type such as Team) BEFORE: c(.x[-n()], NA).
    dplyr::distinct() %>%  ## delete duplicated rows (it is the case of non injured players, the ones that are deleted)
    dplyr::mutate(tstart = dplyr::lag(.data$date_recovered, default = .data$t0[1]),
                  tstop = c(.data$date_injured[seq(length.out = (n() - 1))], .data$tf[n()])) %>% ## same as date_injured[1:(n()-1)]
    dplyr::select("player", "t0", "tf", "date_injured", "date_recovered", "tstart", "tstop", tidyselect::everything()) %>%
    dplyr::ungroup()

  ## Fill tstop for non-injured players and delete those players' observations whose
  ## recovery date is later than the end of her follow-up
  injd <- injd %>%
    dplyr::group_by(.data$player) %>%
    dplyr::mutate_at(vars("tstop"), ~replace(.x, row_number() == 1 & is.na(.x), as.Date(.data$tf)[1])) %>%
    dplyr::filter(.data$tstart <= .data$tstop) %>%
    droplevels() %>%
    dplyr::ungroup()

  ## Obtain minutes (or other units) of exposure
  ## 1) create x suffix, i.e. tstart_x and tstop_x
  unx <- exp_unit_suffix(exp_unit)
  tstart_unx <- paste0("tstart_", unx)
  tstop_unx <- paste0("tstop_", unx)
  injd <- injd %>%
    dplyr::mutate(
      !!(tstart_unx) := 0,
      !!(tstop_unx)  := 0
    )
  ## 2) populate tstop_x column
  if (is.numeric(data_exposures$date)) {
    data_exposures$date_aux <- as.Date(date_format(paste0(data_exposures$date, "-07-01")))
  } else {
    data_exposures$date_aux <- data_exposures$date
  }
  injd <- injd %>%
    dplyr::mutate(!!(tstop_unx) := purrr::pmap_dbl(.l = list(.data$player, .data$tstart, .data$tstop),
                                                   .f = function(playr, tstart, tstop) {
                                                     data_exposures %>%
                                                       dplyr::filter(.data$player %in% playr, .data$date_aux >= tstart, .data$date_aux < tstop) %>%
                                                       dplyr::select("time_expo") %>%
                                                       .[[1]] %>%
                                                       sum()}))

  ## 3) Add enum, status and days_lost columns; edit tstop_min and tstart_min
  injd <- injd %>%
    dplyr::group_by(.data$player) %>%
    dplyr::mutate(
      !!(tstop_unx) := cumsum(.data[[tstop_unx]]),
      !!(tstart_unx) := dplyr::lag(.data[[tstop_unx]], default = 0),
      enum = dplyr::lag(1 * (!is.na(.data$date_injured)), default = 0),
      enum =  cumsum(.data$enum) + 1,
      status = dplyr::if_else(is.na(.data$date_injured), 0, 1),
      days_lost = ifelse(is.na(.data$date_injured),
                         0,
                         as.numeric(difftime(.data$date_recovered, .data$date_injured, units = "days"))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("player", "t0", "tf", "date_injured",
                  "date_recovered", "tstart", "tstop",
                  tidyselect::all_of(tstart_unx), tidyselect::all_of(tstop_unx),
                  "status", "enum", "days_lost",
                  tidyselect::everything())


  ## Create `injd` object
  class(injd) <- c("injd", class(injd))
  attr(injd, "unit_exposure") <- exp_unit
  attr(injd, "follow_up") <- followup_df
  attr(injd, "data_exposures") <- data_exposures %>% select(-"date_aux")
  attr(injd, "data_injuries") <- data_injuries
  return(injd)
}


#' Check if an object is of class \code{injd}
#'
#' Check if an object x is of class \code{injds}.
#'
#' @param x any R object.
#' @export
is_injd <- function(x) inherits(x, "injd")


