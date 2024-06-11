test_that("data_injurieslong works fine", {
  df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")

  data_injuries_long <- data_injurieslong(df_injuries)
  expect_data_frame(data_injuries_long, nrows = 164L, ncols = 3L)
  expect_subset(c("person_id", "event", "date"),
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
                             person_id      = "player_name",
                             date_injured   = "from",
                             date_recovered = "until")
  df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                              person_id        = "player_name",
                              date          = "year",
                              time_expo     = "minutes_played")
  injd <- suppressWarnings(prepare_all(data_exposures = df_exposures,
                                       data_injuries  = df_injuries,
                                       exp_unit = "matches_minutes"))

  injd_cut <- suppressWarnings(cut_injd(injd, date0 = 2018))

  expect_data_frame(injd_cut, nrows = 74L, ncols = 19L)
  expect_s3_class(injd_cut, "injd")
  expect_warning(cut_injd(injd, date0 = 2018), "has been cut")

  injd_cut2 <- suppressWarnings(cut_injd(injd, datef = 2017))
  expect_data_frame(injd_cut2, nrows = 43L, ncols = 19L)

  injd_cut3 <- suppressWarnings(cut_injd(injd, date0 = 2017, datef = 2018))
  expect_data_frame(injd_cut3, nrows = 93L, ncols = 19L)
})

test_that("cut_injd aborts when it has to", {
  expect_error(cut_injd(injd, date0 = 2019, datef = 2018), "date0 must be < datef")
  expect_error(cut_injd(injd, datef = 2016), "Empty")
})

test_that("get_data_followup works fine", {
  follow_up <- get_data_followup(injd)
  expect_data_frame(follow_up, nrows = 53L, ncols = 3L)
  expect_subset(c("person_id", "t0", "tf"),
                names(follow_up))
  expect_identical(data.frame(follow_up[1, ]) |> droplevels(),
                   data.frame(person_id = factor("adam-lallana"),
                              t0 = as.Date("2017-07-01"),
                              tf = as.Date("2019-06-30")))
})

test_that("get_data_injuries works fine", {
  data_injuries <- get_data_injuries(injd)
  expect_data_frame(data_injuries, nrows = 209L, ncols = 3L)
  expect_subset(c("person_id", "date_injured", "date_recovered"),
                names(data_injuries))
  expect_identical(data.frame(data_injuries[1, ]) |> droplevels(),
                   data.frame(person_id = factor("adam-lallana"),
                              date_injured   = as.Date("2017-07-31"),
                              date_recovered = as.Date("2017-11-25")))
})

test_that("get_data_exposures works fine", {
  data_exposures <- get_data_exposures(injd)
  expect_data_frame(data_exposures, nrows = 81L, ncols = 3L)
  expect_subset(c("person_id", "date", "time_expo"),
                names(data_exposures))
  expect_identical(data.frame(data_exposures[1:2, ]) |> droplevels(),
                   data.frame(person_id = factor(rep("adam-lallana", 2)),
                              date = c(as.Date("2017-07-31"), as.Date("2018-09-04")),
                              time_expo = c(236, 464)))
})
