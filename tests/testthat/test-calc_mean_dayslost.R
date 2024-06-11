test_that("calc_mean_daylost works", {
  mean_dayslost <- calc_mean_dayslost(injd)
  expect_vector(mean_dayslost)
  expect_equal(names(mean_dayslost), c("mean_dayslost"))
  expect_identical(round(mean_dayslost[[1]], 3), 16.364)

  mean_dayslost <- calc_mean_dayslost(injd, overall = FALSE)
  expect_data_frame(mean_dayslost, ncol = 2L, nrow = 44L)
  expect_equal(names(mean_dayslost), c("person_id", "mean_dayslost"))

  mean_dayslost <- calc_mean_dayslost(injd, by = "injury_type")
  expect_data_frame(mean_dayslost, ncol = 2L, nrow = 7L)
  expect_equal(names(mean_dayslost), c("injury_type", "mean_dayslost"))

  mean_dayslost <- calc_mean_dayslost(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(mean_dayslost, ncol = 3L, nrow = 116L)
  expect_equal(names(mean_dayslost), c("person_id", "injury_type", "mean_dayslost"))
})
