test_that("calc_ncases works", {
  ncases <- calc_ncases(injd)
  expect_vector(ncases)
  expect_subset(names(ncases), "ncases")
  expect_identical(ncases[[1]], 209)

  ncases <- calc_ncases(injd, overall = FALSE)
  expect_data_frame(ncases, ncol = 2L, nrow = 53L)
  expect_equal(names(ncases), c("person_id", "ncases"))

  ncases <- calc_ncases(injd, by = "injury_type")
  expect_data_frame(ncases, ncol = 2L, nrow = 7L)
  expect_equal(names(ncases), c("injury_type", "ncases"))

  ncases <- calc_ncases(injd, by = "injury_type", overall = FALSE)
  expect_data_frame(ncases, ncol = 3L, nrow = 371L)
  expect_equal(names(ncases), c("person_id", "injury_type", "ncases"))
})
