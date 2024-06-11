test_that("calc_exposure works", {
  totalexpo <- calc_exposure(injd)
  expect_vector(totalexpo)
  expect_subset(names(totalexpo), "totalexpo")
  expect_identical(totalexpo[[1]], 140950)

  totalexpo <- calc_exposure(injd, overall = FALSE)
  expect_data_frame(totalexpo, ncol = 2L, nrow = 53L)
  expect_equal(names(totalexpo), c("person_id", "totalexpo"))

  totalexpo <- calc_exposure(injd, by = "injury_type")
  expect_data_frame(totalexpo, ncol = 2L, nrow = 7L)
  expect_equal(names(totalexpo), c("injury_type", "totalexpo"))

  totalexpo <- calc_exposure(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(totalexpo, ncol = 3L, nrow = 371L)
  expect_equal(names(totalexpo), c("person_id", "injury_type", "totalexpo"))
})
