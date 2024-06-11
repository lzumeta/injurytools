test_that("calc_prevalence works", {
  df_prevalence <- calc_prevalence(injd, time_period = "monthly")
  expect_data_frame(df_prevalence, nrow = 48L, ncol = 6L)
  expect_equal(names(df_prevalence), c("season", "month", "status", "n",
                                       "n_athlete", "prop"))
  expect_equal(levels(df_prevalence$status), c("Available", "Injured"))
  expect_identical(levels(df_prevalence$month), c("Jul", "Aug", "Sep", "Oct",
                                                  "Nov", "Dec", "Jan", "Feb",
                                                  "Mar", "Apr", "May", "Jun"))

  df_prevalence <- calc_prevalence(injd, time_period = "season")
  expect_data_frame(df_prevalence, nrow = 4L, ncol = 5L)

  df_prevalence <- calc_prevalence(injd, time_period = "monthly", by = "injury_type")
  expect_data_frame(df_prevalence, nrow = 132L, ncol = 6L)

  df_prevalence <- calc_prevalence(injd, time_period = "season", by = "injury_type")
  expect_data_frame(df_prevalence, nrow = 16L, ncol = 5L)
})
