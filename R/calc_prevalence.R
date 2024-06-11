#' Calculate prevalence proportion
#'
#' Calculate the prevalence proportion of injured athletes and the proportion of
#' non-injured (available) athletes in the cohort, on a monthly or season basis.
#' Further information on the type of injury may be specified so that the
#' injury-specific prevalences are reported according to this variable.
#'
#' @param injd Prepared data. An \code{injd} object.
#' @param time_period Character. One of "monthly" or "season", specifying the
#'   periodicity according to which to calculate the proportions of available
#'   and injured athletes.
#' @param by Character specifying the name of the column on the basis of which
#'   to classify the injuries and calculate proportions of the injured athletes.
#'   Defaults to \code{NULL}.
#'
#' @return A data frame containing one row for each combination of season, month
#'   (optionally) and injury type (if `by` not specified, then this variable has
#'   two categories: *Available* and *Injured*). Plus, three more columns,
#'   specifying the proportion of athletes (`prop`) satisfying the corresponding
#'   row's combination of values, i.e. prevalence, how many athletes were injured
#'   at that moment with the type of injury of the corresponding row (`n`), over
#'   how many athletes were at that time in the cohort (`n_athlete`). See Note
#'   section.
#'
#' @note If `by` is specified (and not `NULL`), it may happen that an athlete in
#' one month suffers two different types of injuries. For example, a muscle and
#' a ligament injury. In this case, this two injuries contribute to the
#' proportions of muscle and ligament injuries for that month, resulting in an
#' overall proportion that exceeds 100%. Besides, the athletes in Available
#' category are those that did not suffer any injury in that moment
#' (season-month), that is, they were healthy all the time that the period
#' lasted.
#' @export
#'
#' @importFrom dplyr mutate select arrange group_by summarize left_join
#' @importFrom checkmate assert checkClass checkChoice
#' @importFrom stringr str_to_title
#' @importFrom withr with_locale
#' @importFrom lubridate month year
#' @importFrom stringr str_to_title
#' @importFrom tidyselect all_of
#' @importFrom tidyr gather
#'
#' @references Bahr R, Clarsen B, Derman W, et al. International Olympic
#' Committee consensus statement: methods for recording and reporting of
#' epidemiological data on injury and illness in sport 2020 (including STROBE
#' Extension for Sport Injury and Illness Surveillance (STROBE-SIIS))
#' \emph{British Journal of Sports Medicine} 2020; 54:372-389.
#'
#' Nielsen RO, Debes-Kristensen K, Hulme A, et al. Are prevalence measures
#' better than incidence measures in sports injury research? \emph{British
#' Journal of Sports Medicine} 2019; 54:396-397.
#'
#' @examples
#' \donttest{
#' df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
#'                             date = "year", time_expo = "minutes_played")
#' df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
#'                             date_injured = "from", date_recovered = "until")
#' injd         <- prepare_all(data_exposures = df_exposures,
#'                             data_injuries  = df_injuries,
#'                             exp_unit = "matches_minutes")
#' }
#'
#' calc_prevalence(injd, time_period = "monthly", by = "injury_type")
#' calc_prevalence(injd, time_period = "monthly")
#' calc_prevalence(injd, time_period = "season", by = "injury_type")
#' calc_prevalence(injd, time_period = "season")
calc_prevalence <- function(injd,
                            time_period = c("monthly", "season"),
                            by = NULL) {
  assert(checkClass(injd, "injd"),
         checkChoice(time_period, c("monthly", "season")),
         checkClass(by, "character", null.ok = TRUE),
         combine = "and")

  data_injuries <- get_data_injuries(injd)
  ## add season column
  data_injuries$season <- factor(date2season(data_injuries$date_injured))
  seasons <- levels(data_injuries$season)

  if (!is.null(by)) {
    data_injuries <- dplyr::left_join(data_injuries, injd,
                                      by = c("person_id", "date_injured",
                                             "date_recovered"),
                                      suffix = c("", ".y")) |>
      dplyr::mutate(person_id = factor(person_id)) |>
      dplyr::select(c("person_id", "date_injured",
                      "date_recovered", "season", tidyselect::all_of(by)))
  }

  if (time_period == "monthly") {
    # create empty data frame
    df_prev <- data.frame(person_id = vector("character"),
                          month = vector("character"),
                          status = vector("character"),
                          season = vector("character"),
                          id_injury = vector("numeric"))

    j <- 1
    for (i in seq_len(nrow(data_injuries))) {
      person_id <- data_injuries$person_id[[i]] |> as.character()
      date_injured <- data_injuries$date_injured[[i]]
      month_injured <- withr::with_locale(c("LC_TIME" = "C"),
                                          lubridate::month(date_injured, label = T))

      if (is.null(by)) {
        status <- "Injured"
      } else {
        status <- data_injuries[[by]][[i]] |> as.character()
      }
      season <- data_injuries$season[[i]] |> as.character()
      id_injury <- i

      df_prev[j, ] <- data.frame(person_id = person_id,
                                 month_injured = as.character(month_injured),
                                 status = status,
                                 season = season,
                                 id_injury = id_injury)

      ## extract recovery date
      date_recovered <- data_injuries$date_recovered[[i]]

      ## cut the recovery date to the follow-up date (for those players with long-term injuries)
      if (date2season(date_recovered) > max(seasons)) {
        date_recovered <- as.Date(paste0(season2year(max(seasons)), "-06-30", format = "%Y-%m-%d"))
        date_recovered <- date_format(date_recovered)
      }

      ## extract months and years
      month_recovered <- withr::with_locale(c("LC_TIME" = "C"),
                                            lubridate::month(date_recovered, label = T))
      year_recovered <- lubridate::year(date_recovered)
      year_injured <- lubridate::year(date_injured)

      month <- month_injured
      while (month < month_recovered || (month > month_recovered && year_injured < year_recovered)) {
        j <- j + 1
        month <- withr::with_locale(c("LC_TIME" = "C"),
                                    lubridate::month((as.numeric(month) %% 12 + 1), label = T))
        year_season <- season2year(season)
        if (((year_season + 1) == year_injured && as.numeric(month_injured) < 7 && as.numeric(month) >= 7) || ## same year different seasons
            (year_season == year_injured && year_season < year_recovered && as.numeric(month_recovered) >= 7 && as.numeric(month) >= 7)) { ## different year and season
          season <- seasons[which(season %in% seasons) + 1]
        }
        df_prev[j, ] <- cbind(person_id = person_id,
                              month = as.character(month),
                              status = status,
                              season = season,
                              id_injury = id_injury)

      }
      j <- j + 1
    }

    data_followup <- get_data_followup(injd) |>
      dplyr::mutate(season0 = factor(date2season(.data$t0)),
                    seasonf = factor(date2season(.data$tf))) |>
      tidyr::gather(key = "type_season", value = "season",
                    -c("person_id", "t0", "tf")) |>
      dplyr::arrange("person_id") |>
      dplyr::select(-"type_season") |>
      dplyr::mutate(season = factor(.data$season)) |>
      unique() |>
      dplyr::select("person_id", "season") |>
      unique()

    ## expand to twelve month each season that each person_id has
    data_followup <- purrr::map_dfr(seq_len(12), ~data_followup) |>
      dplyr::arrange(person_id, season) |>
      dplyr::mutate(month = rep(c("Jul", "Aug", "Sep", "Oct", "Nov",
                                  "Dec", "Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun"), times = nrow(data_followup)))

    df_prev <- dplyr::right_join(df_prev, data_followup,
                                 by = c("person_id", "season", "month")) |>
      dplyr::mutate(season = factor(season, levels = levels(data_followup$season)),
                    month = factor(stringr::str_to_title(month),
                                   levels = c("Jul", "Aug", "Sep", "Oct", "Nov",
                                              "Dec", "Jan", "Feb", "Mar", "Apr",
                                              "May", "Jun")),
                    person_id = factor(person_id),
                    status = ifelse(is.na(status), "Available", status),
                    status = stats::relevel(factor(status), ref = "Available"),
                    season = factor(season)) |>
      dplyr::group_by(season, month, status, person_id) |>  ## important step
      unique() |>
      dplyr::ungroup()

    if (is.null(by)) {
      df_prev <- df_prev |>
        dplyr::select(-"id_injury") |>
        unique()
    }

    ## Calculate proportions
    df_prev <- df_prev |>
      dplyr::group_by(season, month) |>
      dplyr::mutate(n_athlete = dplyr::n_distinct(person_id)) |>
      dplyr::ungroup() |>
      dplyr::group_by(season, month, status) |>
      dplyr::summarise(n = dplyr::n(),
                       n_athlete = dplyr::first(.data$n_athlete)) |>
      dplyr::ungroup() |>
      dplyr::mutate(prop = round(.data$n / .data$n_athlete * 100, 1),
                    season = factor(paste0("season ", as.character(season))))
  }

  if (time_period == "season") {
    # create empty data frame
    df_prev <- data.frame(person_id = vector("character"),
                          status = vector("character"),
                          season = vector("character"),
                          id_injury = vector("numeric"))

    j <- 1
    for (i in seq_len(nrow(data_injuries))) {
      person_id <- data_injuries$person_id[[i]] |> as.character()
      if (is.null(by)) {
        status <- "Injured"
      } else {
        status <- data_injuries[[by]][[i]] |> as.character()
      }
      season <- data_injuries$season[[i]] |> as.character()
      id_injury <- i

      df_prev[j, ] <- data.frame(person_id = person_id,
                                 status = status,
                                 season = season,
                                 id_injury = id_injury)

      ## extract recovery date
      date_recovered <- data_injuries$date_recovered[[i]]

      ## cut the recovery date to the follow-up date (for those players with long-term injuries)
      if (date2season(date_recovered) > max(seasons)) {
        date_recovered <- as.Date(paste0(season2year(max(seasons)), "-06-30", format = "%Y-%m-%d"))
        date_recovered <- date_format(date_recovered)
      }

      ## extract years
      year_recovered <- lubridate::year(date_recovered)
      season_recovered <- date2season(date_recovered)
      year_injured <- lubridate::year(data_injuries$date_injured[[i]])

      while (season < season_recovered) {
        j <- j + 1
        season <- seasons[which(season %in% seasons) + 1]
        df_prev[j, ] <- cbind(person_id = person_id,
                              status = status,
                              season = season,
                              id_injury = id_injury)

      }
      j <- j + 1
    }

    data_followup <- get_data_followup(injd) |>
      dplyr::mutate(season0 = factor(date2season(.data$t0)),
                    seasonf = factor(date2season(.data$tf))) |>
      tidyr::gather(key = "type_season", value = "season",
                    -c("person_id", "t0", "tf")) |>
      dplyr::arrange("person_id") |>
      dplyr::select(-"type_season") |>
      dplyr::mutate(season = factor(.data$season)) |>
      unique() |>
      dplyr::select("person_id", "season") |>
      unique()

    df_prev <- dplyr::right_join(df_prev, data_followup,
                                 by = c("person_id", "season")) |>
      dplyr::mutate(season = factor(season, levels = levels(data_followup$season)),
                    person_id = factor(person_id),
                    status = ifelse(is.na(status), "Available", status),
                    status = stats::relevel(factor(status), ref = "Available"),
                    season = factor(season)) |>
      dplyr::group_by(season, status, person_id) |>  ## important step
      unique() |>
      dplyr::ungroup()

    if (is.null(by)) {
      df_prev <- df_prev |>
        dplyr::select(-"id_injury") |>
        unique()
    }

    ## Calculate proportions
    df_prev <- df_prev |>
      dplyr::group_by(season) |>
      dplyr::mutate(n_athlete = dplyr::n_distinct(person_id)) |>
      dplyr::ungroup() |>
      dplyr::group_by(season, status) |>
      dplyr::summarise(n = dplyr::n(),
                       n_athlete = dplyr::first(.data$n_athlete)) |>
      dplyr::ungroup() |>
      dplyr::mutate(prop = round(.data$n / .data$n_athlete * 100, 1),
                    season = factor(paste0("season ", as.character(season))))

  }

  return(df_prev)
}
