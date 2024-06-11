test_that("prepare_inj works", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")

  expect_data_frame(df_injuries, nrows = 82L, ncols = 11L)
  expect_data_frame(df_injuries,
                    nrow = nrow(raw_df_injuries), ncol = ncol(raw_df_injuries))
  expect_identical(class(df_injuries[["date_injured"]]), "Date")
  expect_identical(class(df_injuries[["date_recovered"]]), "Date")

  raw_df_injuries$until[[1]] <- raw_df_injuries$from[[1]] - 2
  expect_error(prepare_inj(df_injuries0   = raw_df_injuries,
                           person_id      = "player_name",
                           date_injured   = "from",
                           date_recovered = "until"),
               "date_recovered should be")
})


test_that("prepare_exp works", {
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id     = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  expect_data_frame(df_exposures, nrows = 42L, ncols = 19L)
  expect_data_frame(df_exposures,
                    nrow = nrow(raw_df_exposures), ncol = ncol(raw_df_exposures))
  expect_subset(class(df_exposures[["date"]]), c("Date", "numeric"))
  expect_subset(class(df_exposures[["time_expo"]]), c("numeric", "integer"))

  raw_df_exposures$year[[1]] <- 202
  expect_error(prepare_exp(df_exposures0 = raw_df_exposures,
                           person_id     = "player_name",
                           date          = "year",
                           time_expo     = "minutes_played"),
               "it must refer to the year")
})


test_that("prepare_all works", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id     = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  injd <- suppressWarnings(prepare_all(data_exposures = df_exposures,
                                       data_injuries  = df_injuries,
                                       exp_unit = "matches_minutes"))
  expect_equal(is.data.frame(injd), TRUE)
  expect_s3_class(injd, "injd")
  expect_subset(c("person_id", "t0", "tf", "date_injured", "date_recovered",
                  "tstart", "tstop", "status", "enum"),
                names(injd))
  expect_data_frame(data.frame(injd), nrows = 108L, ncols = 19L)
  expect_type(attr(injd, "unit_exposure"), "character")
  expect_equal(is_injd(injd), TRUE)

  expect_error(prepare_all(data_exposures = df_exposures,
                           data_injuries  = df_injuries,
                           exp_unit = "matches"))
})


test_that("prepare_all works fine when dates in injury and exposure data do not match", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id     = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  data_exposures <- filter(df_exposures, date == 2018)

  # Should have 1 warning
  expect_snapshot({
    out <- prepare_all(data_exposures = data_exposures,
                       data_injuries  = df_injuries,
                       exp_unit = "matches_minutes")
  })
})

test_that("data_followup works as expected", {
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id     = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  follow_up <- data_followup(df_exposures)
  expect_data_frame(follow_up, nrows = 28L, ncols = 3L)
  expect_identical(class(follow_up$t0), "Date")
  expect_identical(class(follow_up$tf), "Date")

  df_exposures$date[[1]] <- 201
  expect_error(data_followup(df_exposures), "it must refer to the year")
  df_exposures$date[[1]] <- as.Date("2019-01-09")
  expect_error(data_followup(df_exposures), "it must refer to the year")
})


