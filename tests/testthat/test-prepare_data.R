test_that("prepare_inj works", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             player         = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")

  expect_equal(is.data.frame(df_injuries), TRUE)
  expect_data_frame(df_injuries,
                    nrow = nrow(raw_df_injuries), ncol = ncol(raw_df_injuries))
  expect_identical(class(df_injuries[["date_injured"]]), "Date")
  expect_identical(class(df_injuries[["date_recovered"]]), "Date")
})


test_that("prepare_exp works", {
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              player        = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  expect_equal(is.data.frame(df_exposures), TRUE)
  expect_data_frame(df_exposures,
                    nrow = nrow(raw_df_exposures), ncol = ncol(raw_df_exposures))
  expect_subset(class(df_exposures[["date"]]), c("Date", "numeric"))
  expect_subset(class(df_exposures[["time_expo"]]), c("numeric", "integer"))
})


test_that("prepare_all works", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             player         = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              player        = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  injd <- suppressWarnings(prepare_all(data_exposures = df_exposures,
                      data_injuries  = df_injuries,
                      exp_unit = "matches_minutes"))
  expect_equal(is.data.frame(injd), TRUE)
  expect_s3_class(injd, "injd")
  expect_subset(c("player", "t0", "tf", "date_injured", "date_recovered",
                  "tstart", "tstop", "status", "enum"),
                names(injd))
  expect_type(attr(injd, "unit_exposure"), "character")
  expect_equal(is.data.frame(attr(injd, "follow_up")), TRUE)
  expect_equal(is.data.frame(attr(injd, "data_exposures")), TRUE)
  expect_equal(is.data.frame(attr(injd, "data_injuries_long")), TRUE)
  expect_equal(is.data.frame(attr(injd, "data_injuries")), TRUE)
  expect_equal(is_injd(injd), TRUE)
})

test_that("when dates in injury and exposure data do not match, prepare_all works fine", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             player         = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              player        = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")

  data_exposures <- filter(df_exposures, date == 2018)
  w <- capture_warnings(prepare_all(data_exposures = data_exposures,
                                          data_injuries  = df_injuries,
                                          exp_unit = "matches_minutes"))
  expect_length(w, 1)
  expect_match(w, regexp = "Injury data has been cut")

})
