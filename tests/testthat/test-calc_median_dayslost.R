test_that("calc_median_daylost works", {
  median_dayslost <- calc_median_dayslost(injd)
  expect_vector(median_dayslost)
  expect_equal(names(median_dayslost), c("median_dayslost"))
  expect_equal(median_dayslost[[1]], 7)

  median_dayslost <- calc_median_dayslost(injd, overall = FALSE)
  expect_data_frame(median_dayslost, ncol = 2L, nrow = 44L)
  expect_equal(names(median_dayslost), c("person_id", "median_dayslost"))

  median_dayslost <- calc_median_dayslost(injd, by = "injury_type")
  expect_data_frame(median_dayslost, ncol = 2L, nrow = 7L)
  expect_equal(names(median_dayslost), c("injury_type", "median_dayslost"))

  median_dayslost <- calc_median_dayslost(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(median_dayslost, ncol = 3L, nrow = 116L)
  expect_equal(names(median_dayslost), c("person_id", "injury_type", "median_dayslost"))
})
