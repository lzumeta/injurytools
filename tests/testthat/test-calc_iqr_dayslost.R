test_that("calc_iqr_daylost works", {
  iqr_dayslost <- calc_iqr_dayslost(injd)
  expect_vector(iqr_dayslost)
  expect_equal(names(iqr_dayslost), c("qt25_dayslost", "qt75_dayslost"))
  expect_identical(iqr_dayslost[[1]], 4)
  expect_identical(iqr_dayslost[[2]], 16)

  iqr_dayslost <- calc_iqr_dayslost(injd, overall = FALSE)
  expect_data_frame(iqr_dayslost, ncol = 3L, nrow = 44L)
  expect_equal(names(iqr_dayslost), c("person_id", "qt25_dayslost", "qt75_dayslost"))

  iqr_dayslost <- calc_iqr_dayslost(injd, by = "injury_type")
  expect_data_frame(iqr_dayslost, ncol = 3L, nrow = 7L)
  expect_equal(names(iqr_dayslost), c("injury_type", "qt25_dayslost", "qt75_dayslost"))

  iqr_dayslost <- calc_iqr_dayslost(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(iqr_dayslost, ncol = 4L, nrow = 116L)
  expect_equal(names(iqr_dayslost), c("person_id", "injury_type", "qt25_dayslost", "qt75_dayslost"))
})
