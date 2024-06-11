test_that("calc_ndaylost works", {
  ndayslost <- calc_ndayslost(injd)
  expect_vector(ndayslost)
  expect_subset(names(ndayslost), "ndayslost")
  expect_identical(ndayslost[[1]], 3420)

  ndayslost <- calc_ndayslost(injd, overall = FALSE)
  expect_data_frame(ndayslost, ncol = 2L, nrow = 53L)
  expect_equal(names(ndayslost), c("person_id", "ndayslost"))

  ndayslost <- calc_ndayslost(injd, by = "injury_type")
  expect_data_frame(ndayslost, ncol = 2L, nrow = 7L)
  expect_equal(names(ndayslost), c("injury_type", "ndayslost"))

  ndayslost <- calc_ndayslost(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(ndayslost, ncol = 3L, nrow = 371L)
  expect_equal(names(ndayslost), c("person_id", "injury_type", "ndayslost"))
})
