test_that("calc_summary works", {
  summary_data <- calc_summary(injd, quiet = TRUE)

  expect_equal(is.data.frame(summary_data), TRUE)
  expect_data_frame(summary_data, nrows = 1L, ncols = 15L)

  expect_subset(c('totalexpo', 'ncases', 'incidence',
                  'incidence_sd', 'incidence_lower', 'incidence_upper',
                  'ndayslost', 'mean_dayslost', 'median_dayslost',
                  'qt25_dayslost', 'qt75_dayslost', 'burden',
                  'burden_sd', 'burden_lower', 'burden_upper'),
                names(summary_data))

  ## works when specifying 'by' arg
  summary_data <- calc_summary(injd, by = "injury_type", quiet = TRUE)

  expect_equal(is.data.frame(summary_data), TRUE)
  expect_data_frame(summary_data, nrows = 7L, ncols = 18L)

  expect_subset(c("injury_type", "percent_ncases", "percent_ndayslost"),
                names(summary_data))

  ## works when specifying 'overall' arg to FALSE
  summary_data <- calc_summary(injd, overall = FALSE, quiet = TRUE)

  expect_equal(is.data.frame(summary_data), TRUE)
  expect_data_frame(summary_data, nrows = 53L, ncols = 16L)

  expect_subset(c("person_id"),
                names(summary_data))

  ## works when specifying 'by' arg and 'overall' arg to FALSE
  summary_data <- calc_summary(injd, by = "injury_type",
                               overall = FALSE, quiet = TRUE)

  expect_equal(is.data.frame(summary_data), TRUE)
  expect_data_frame(summary_data, nrows = 371L, ncols = 17L)

  expect_subset(c("injury_type", "person_id"),
                names(summary_data))
})

test_that("warnings are working", {

  w <- capture_warnings(calc_summary(injd))
  expect_length(w, 2)
  expect_match(w, regexp = "Exposure time unit is")

  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id     = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")
  df_exposures[df_exposures$person_id == df_exposures$person_id[[1]], "time_expo"] <- 0
  injd <- suppressWarnings(prepare_all(data_exposures = df_exposures,
                                       data_injuries  = df_injuries,
                                       exp_unit = "matches_minutes"))
  w <- capture_warnings(calc_summary(injd))
  expect_length(w, 2)
  expect_match(w, regexp = "(There are athletes)|(Exposure time unit is)")


  expect_error(calc_data(injds, by = "type_injury"))
})

test_that("summary_unit works", {
  ## matches minutes
  ws <- capture_warnings(calc_summary(injd))
  expect_match(ws, " Exposure time unit is matches_minutes")

  injdv2 <- injd
  attr(injdv2, "unit_exposure") <- "activity_days"
  ws2 <- capture_warnings(calc_summary(injdv2))
  expect_match(ws2[[1]], "Exposure time unit is activity_days", fixed = TRUE)
})


