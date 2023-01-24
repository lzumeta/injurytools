test_that("data_injurieslong works fine", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             player         = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")

  data_injuries_long <- data_injurieslong(df_injuries)
  expect_data_frame(data_injuries_long, nrows = 164L, ncols = 3L)
  expect_subset(c("player", "event", "date"),
                names(data_injuries_long))
})

test_that("date2season works", {
  date <- as.Date("2023-01-09")
  expect_identical(date2season(date), "2022/2023")

  date2 <- as.Date("2023-07-01")
  expect_identical(date2season(date2), "2023/2024")
})

test_that("season2year works", {
  season <- "2022/2023"
  expect_identical(season2year(season), 2022)

  season2 <- "2023/2024"
  expect_identical(season2year(season2), 2023)
})

test_that("cut_injd works", {
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

  injd_cut <- suppressWarnings(cut_injd(injd, date0 = 2018))

  expect_data_frame(injd_cut, nrow = 74L, ncols = 19L)
  expect_s3_class(injd_cut, "injd")
  expect_warning(cut_injd(injd, date0 = 2018), "has been cut")
})

test_that("cut_injd aborts when it has to", {
  expect_error(cut_injd(injd, date0 = 2019, datef = 2018), "date0 must be < datef")
  expect_error(cut_injd(injd, date0 = as.Date("2018-01-09"), datef = 2018), "must be of the same class")
  expect_error(cut_injd(injd, datef = 2016), "Empty")
  expect_error(cut_injd(injd, datef = as.Date("2018-01-09")), "datef and data_exposures")
})
