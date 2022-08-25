## code to prepare `DATASET` dataset goes here
library(dplyr)
library(injurytools)

raw_df_exposures <- readRDS("../Injury-Scraping_data/players_longitudinal_df.Rds")
raw_df_injuries <- readRDS("../Injury-Scraping_data/injuries_df.Rds")

years2select <- c(2017, 2018)
teams2select <- c("fc-liverpool")
# teams2select <- c("fc-bayern-munchen", "fc-liverpool", "fc-barcelona", "real-madrid")

raw_df_exposures <- raw_df_exposures %>%
  filter(club_name %in% teams2select,
         year %in% years2select,
         matches_played > 5) %>%
  droplevels() %>%
  select(player_name, player_id, season, year, matches_played, minutes_played,
         liga, club_name, club_id, age, place, position, foot, goals, assists, reds)
raw_df_injuries <- raw_df_injuries %>%
  inner_join(raw_df_exposures %>% select(player_id, year), by = c("player_id" = "player_id",
                                                                  "year" = "year")) %>%
  droplevels() %>%
  select(player_name, player_id, season, from, until, days_lost,
         games_lost, injury, injury_acl, injury_type, injury_severity)

usethis::use_data(raw_df_injuries, raw_df_exposures, overwrite = TRUE)


## internal data
df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                           player         = "player_name",
                           date_injured   = "from",
                           date_recovered = "until")

df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                            player        = "player_name",
                            date          = "year",
                            time_expo     = "minutes_played")

injd <- prepare_all(data_exposures = df_exposures,
                    data_injuries  = df_injuries,
                    exp_unit = "matches_minutes")

usethis::use_data(injd, overwrite = TRUE)


# deprecated:
# df_exposures <- df_exposures %>%
#   dplyr::filter(club_name == "fc-bayern-munchen")
# df_injuries  <- df_injuries %>%
#   dplyr::left_join(df_exposures %>% dplyr::select(player_id, season),
#                    by = c("player_id", "season"))
# injd2 <- prepare_all(data_exposures = df_exposures,
#                      data_injuries  = df_injuries,
#                      exp_unit = "matches_minutes")

# usethis::use_data(injd, injd2, overwrite = TRUE, internal = TRUE)
