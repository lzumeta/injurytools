#' Title
#'
#' @param injd Prepared data, an \code{injd} object.
#' @param by FILL
#' @param var_type_injury Character specifying the name of the column on the
#'                        basis of which to classify the injuries and calculate
#'                        proportions of the injured players. Defaults to
#'                        \code{NULL}.
#'
#' @return FILL
#' @export
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
#' injprop(injd, by = "monthly", var_type_injury = "injury_type")
injprop <- function(injd, by = c("monthly", "season"), var_type_injury) {
  assert(checkClass(injd, "injd"))

  data_injuries <- attr(injd, "data_injuries")

  df_polar <- data.frame(player = vector("character"),
                         month = vector("character"),
                         type_injury = vector("character"),
                         season = vector("character"),
                         id_injury = vector("numeric"))

  seasons <- levels(data_injuries$season)  ## CHECK!! (it may not have seasons)

  j <- 1
  for (i in 1:nrow(data_injuries)) {
    player <- data_injuries$player[[i]] %>% as.character()
    month <- withr::with_locale(c("LC_TIME" = "C"),
                                lubridate::month(data_injuries$date_injured[[i]], label = T))
    type_injury <- data_injuries[[var_type_injury]][[i]] %>% as.character()
    season <- data_injuries$season[[i]] %>% as.character()
    id_injury <- i

    df_polar[j, ] <- data.frame(player = player,
                                month = as.character(month),
                                type_injury = type_injury,
                                season = season,
                                id_injury = id_injury)

    month_recovered <- withr::with_locale(c("LC_TIME" = "C"),
                                          lubridate::month(data_injuries$date_recovered[[i]], label = T))
    year_recovered <- lubridate::year(data_injuries$date_recovered[[i]])
    year_injured <- lubridate::year(data_injuries$date_injured[[i]])
    while(month < month_recovered | (month > month_recovered & year_injured < year_recovered)) {
      j <- j + 1
      month <- withr::with_locale(c("LC_TIME" = "C"),
                                  lubridate::month((as.numeric(month)%%12 + 1), label = T))
      if (year_injured < year_recovered & as.numeric(month) >= 7) season <- seasons[which(season %in% seasons) + 1]
      df_polar[j, ] <- cbind(player = player,
                             month = as.character(month),
                             type_injury = type_injury,
                             season = season,
                             id_injury = id_injury)

    }
    j <- j + 1
  }


  data_exposures <- attr(injd, "data_exposures") %>%
    dplyr::select(player, season) %>%
    unique()
  ## expand t twelve month each season that each player has
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

  return(df_polar)
}
