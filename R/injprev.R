#' Calculate injury prevalence
#'
#' Calculate the prevalence of injured players and the proportion of non-injured
#' (available) players in the cohort, on a monthly or season basis. Further
#' information on the type of injury may be specified so that the
#' injury-specific prevalences are reported according to this variable.
#'
#' @param injd Prepared data. An \code{injd} object.
#' @param by Character. One of "monthly" or "season", specifying the periodicity
#'   according to which to calculate the proportions of available and injured
#'   players/athletes.
#' @param var_type_injury Character specifying the name of the column on the
#'   basis of which to classify the injuries and calculate proportions of the
#'   injured players. Defaults to \code{NULL}.
#'
#' @return A data frame containing one row for each combination of season, month
#'   (optionally) and injury type (if `var_type_injury` not specified, then this
#'   variable has two categories: *Available* and *Injured*). Plus, three more
#'   columns, specifying the proportion of players (`prop`) satisfying the
#'   corresponding row's combination of values, i.e. prevalence, how many
#'   players were injured at that moment with the type of injury of the
#'   corresponding row (`n`), over how many players were at that time in the
#'   cohort (`n_player`). See Note section.
#'
#' @note
#' If `var_type_injury` is specified (and not `NULL`), it may happen that a
#' player in one month suffers two different types of injuries. For example, a
#' muscle and a ligament injury. In this case, this two injuries contribute to
#' the proportions of muscle and ligament injuries for that month, resulting in
#' an overall proportion that exceeds 100%. Besides, the players in Available
#' category are those that did not suffer any injury in that moment
#' (season-month), that is, they were healthy all the time that the period
#' lasted
#' @export
#'
#' @importFrom dplyr mutate select arrange group_by summarize
#' @importFrom checkmate assert checkClass checkChoice
#'
#' @references
#' Bahr R, Clarsen B, Derman W, et al. International Olympic Committee
#' consensus statement: methods for recording and reporting of epidemiological
#' data on injury and illness in sport 2020 (including STROBE Extension for
#' Sport Injury and Illness Surveillance (STROBE-SIIS)) \emph{British Journal of
#' Sports Medicine} 2020; 54:372-389.
#'
#'
#' @examples
#' # df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#' #                             date = "year", time_expo = "minutes_played")
#' # df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#' #                             date_injured = "from", date_recovered = "until")
#' # injd         <- prepare_all(data_exposures = df_exposures,
#' #                             data_injuries  = df_injuries,
#' #                             exp_unit = "matches_minutes")
#'
#' injprev(injd, by = "monthly", var_type_injury = "injury_type")
#' injprev(injd, by = "monthly")
#' injprev(injd, by = "season", var_type_injury = "injury_type")
#' injprev(injd, by = "season")
injprev <- function(injd, by = c("monthly", "season"), var_type_injury = NULL) {
  assert(checkClass(injd, "injd"),
         checkChoice(by, c("monthly", "season")),
         checkClass(var_type_injury, "character", null.ok = TRUE),
         combine = "and")

  data_injuries <- attr(injd, "data_injuries")
  ## add season column
  data_injuries$season <- factor(date2season(data_injuries$date_injured))
  seasons <- levels(data_injuries$season)

  if (by == "monthly") {
    # create empty data frame
    df_polar <- data.frame(player = vector("character"),
                           month = vector("character"),
                           type_injury = vector("character"),
                           season = vector("character"),
                           id_injury = vector("numeric"))

    j <- 1
    for (i in 1:nrow(data_injuries)) {
      player <- data_injuries$player[[i]] %>% as.character()
      date_injured <- data_injuries$date_injured[[i]]
      month_injured <- withr::with_locale(c("LC_TIME" = "C"),
                                          lubridate::month(date_injured, label = T))

      if (is.null(var_type_injury)) {
        type_injury <- "Injured"
      } else {
        type_injury <- data_injuries[[var_type_injury]][[i]] %>% as.character()
      }
      season <- data_injuries$season[[i]] %>% as.character()
      id_injury <- i

      df_polar[j, ] <- data.frame(player = player,
                                  month_injured = as.character(month_injured),
                                  type_injury = type_injury,
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
      while(month < month_recovered | (month > month_recovered & year_injured < year_recovered)) {
        j <- j + 1
        month <- withr::with_locale(c("LC_TIME" = "C"),
                                    lubridate::month((as.numeric(month)%%12 + 1), label = T))
        year_season <- season2year(season)
        if (((year_season+1) == year_injured & as.numeric(month_injured) < 7 & as.numeric(month) >= 7) | ## same year different seasons
            (year_season == year_injured & year_season < year_recovered & as.numeric(month_recovered) >= 7 & as.numeric(month) >= 7)) { ## different year and season
          season <- seasons[which(season %in% seasons) + 1]
        }
        df_polar[j, ] <- cbind(player = player,
                               month = as.character(month),
                               type_injury = type_injury,
                               season = season,
                               id_injury = id_injury)

      }
      j <- j + 1
    }

    data_exposures <- attr(injd, "data_exposures") %>%
      dplyr::mutate(season = factor(date2season(.data$date))) %>%
      dplyr::select(player, season) %>%
      unique()
    ## expand to twelve month each season that each player has
    data_exposures <- purrr::map_dfr(seq_len(12), ~data_exposures) %>%
      dplyr::arrange(player, season) %>%
      dplyr::mutate(month = rep(c("Jul", "Aug", "Sep", "Oct", "Nov",
                                  "Dec", "Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun"), times = nrow(data_exposures)))

    df_polar <- dplyr::right_join(df_polar, data_exposures,
                                  by = c("player", "season", "month")) %>%
      dplyr::mutate(season = factor(season, levels = levels(data_exposures$season)),
                    month = factor(stringr::str_to_title(month),
                                   levels = c("Jul", "Aug", "Sep", "Oct", "Nov",
                                              "Dec", "Jan", "Feb", "Mar", "Apr",
                                              "May", "Jun")),
                    player = factor(player),
                    type_injury = ifelse(is.na(type_injury), "Available", type_injury),
                    type_injury = stats::relevel(factor(type_injury), ref = "Available"),
                    season = factor(season)) %>%
      dplyr::group_by(season, month, type_injury, player) %>%  ## important step
      unique() %>%
      dplyr::ungroup()

    if (is.null(var_type_injury)) {
      df_polar <- df_polar %>%
        dplyr::select(-"id_injury") %>%
        unique()
    }

    ## Calculate proportions
    df_polar <- df_polar %>%
      dplyr::group_by(season, month) %>%
      dplyr::mutate(n_player = dplyr::n_distinct(player)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(season, month, type_injury) %>%
      dplyr::summarise(n = dplyr::n(),
                       n_player = dplyr::first(.data$n_player)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prop = round(.data$n/.data$n_player*100,1),
                    season = factor(paste0("season ", as.character(season))))
  }

  if (by == "season") {
    # create empty data frame
    df_polar <- data.frame(player = vector("character"),
                           type_injury = vector("character"),
                           season = vector("character"),
                           id_injury = vector("numeric"))

    j <- 1
    for (i in 1:nrow(data_injuries)) {
      player <- data_injuries$player[[i]] %>% as.character()
      if (is.null(var_type_injury)) {
        type_injury <- "Injured"
      } else {
        type_injury <- data_injuries[[var_type_injury]][[i]] %>% as.character()
      }
      season <- data_injuries$season[[i]] %>% as.character()
      id_injury <- i

      df_polar[j, ] <- data.frame(player = player,
                                  type_injury = type_injury,
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

      while(season < season_recovered) {
        j <- j + 1
        season <- seasons[which(season %in% seasons) + 1]
        df_polar[j, ] <- cbind(player = player,
                               type_injury = type_injury,
                               season = season,
                               id_injury = id_injury)

      }
      j <- j + 1
    }

    data_exposures <- attr(injd, "data_exposures") %>%
      dplyr::mutate(season = factor(date2season(.data$date))) %>%
      dplyr::select(player, season) %>%
      unique()

    df_polar <- dplyr::right_join(df_polar, data_exposures,
                                  by = c("player", "season")) %>%
      dplyr::mutate(season = factor(season, levels = levels(data_exposures$season)),
                    player = factor(player),
                    type_injury = ifelse(is.na(type_injury), "Available", type_injury),
                    type_injury = stats::relevel(factor(type_injury), ref = "Available"),
                    season = factor(season)) %>%
      dplyr::group_by(season, type_injury, player) %>%  ## important step
      unique() %>%
      dplyr::ungroup()

    if (is.null(var_type_injury)) {
      df_polar <- df_polar %>%
        dplyr::select(-"id_injury") %>%
        unique()
    }

    ## Calculate proportions
    df_polar <- df_polar %>%
      dplyr::group_by(season) %>%
      dplyr::mutate(n_player = dplyr::n_distinct(player)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(season, type_injury) %>%
      dplyr::summarise(n = dplyr::n(),
                       n_player = dplyr::first(.data$n_player)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prop = round(.data$n/.data$n_player*100,1),
                    season = factor(paste0("season ", as.character(season))))

  }

  return(df_polar)
}
