test_that("injsummary works", {
  injds <- injsummary(injd, quiet = TRUE)

  expect_equal(is.list(injds), TRUE)
  expect_identical(length(injds), 2L)
  expect_s3_class(injds, "injds")
  expect_equal(is_injds(injds), TRUE)
  expect_data_frame(injds[[1]], nrows = 53L, ncols = 9L)
  expect_data_frame(injds[[2]], nrows = 1L, ncols = 14L)
  expect_equal(attr(injds, "unit_exposure"), "matches_minutes")
  expect_equal(attr(injds, "unit_timerisk"), "100 player-match")
  expect_subset(c("playerwise", "overall"), names(injds))

  expect_subset(c("player", "ninjuries", "ndayslost", "mean_dayslost",
                  "median_dayslost", "iqr_dayslost", "totalexpo",
                  "injincidence", "injburden"),
                names(injds[[1]]))
  expect_subset(c("ninjuries", "ndayslost", "mean_dayslost", "median_dayslost",
                  "iqr_dayslost", "totalexpo", "injincidence", "injburden",
                  "injincidence_sd", "injburden_sd", "injincidence_lower",
                  "injincidence_upper", "injburden_lower", "injburden_upper"),
                names(injds[[2]]))

  ## works when specifying 'var_type_injury' arg
  injds <- injsummary(injd, var_type_injury = "injury_type", quiet = TRUE)

  expect_equal(is.list(injds), TRUE)
  expect_identical(length(injds), 2L)
  expect_s3_class(injds, "injds")
  expect_equal(is_injds(injds), TRUE)
  expect_data_frame(injds[[1]], nrows = 371L, ncols = 10L)
  expect_data_frame(injds[[2]], nrows = 7L, ncols = 17L)
  expect_subset(c("playerwise", "overall"), names(injds))

  expect_subset(c("injury_type"),
                names(injds[[1]]))
  expect_subset(c("injury_type", "percent_ninjuries", "percent_dayslost"),
                names(injds[[2]]))
})

test_that("warnings are working", {

  injds <- injsummary(injd, quiet = TRUE)

  w <- capture_warnings(injsummary(injd))
  expect_length(w, 2)
  expect_match(w, regexp = "Exposure time unit is")

  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             player         = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              player        = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")
  df_exposures[df_exposures$player == df_exposures$player[[1]], "time_expo"] <- 0
  injd <- suppressWarnings(prepare_all(data_exposures = df_exposures,
                                       data_injuries  = df_injuries,
                                       exp_unit = "matches_minutes"))
  w <- capture_warnings(injsummary(injd))
  expect_length(w, 3)
  expect_match(w, regexp = "(There are players)|(Exposure time unit is)")

  expect_error(injsummary(injds, var_type_injury = "type_injury"))
})

test_that("injsummary_unit works", {
  ## matches minutes
  ws <- capture_warnings(injsummary(injd))
  expect_match(ws, " Exposure time unit is matches_minutes")

  injdv2 <- injd
  attr(injdv2, "unit_exposure") <- "activity_days"
  ws2 <- capture_warnings(injsummary(injdv2))
  expect_match(ws2[[1]], "Exposure time unit is activity_days", fixed = TRUE)
})
