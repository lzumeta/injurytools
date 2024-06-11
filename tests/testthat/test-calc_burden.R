test_that("calc_burden works", {
  df_burden <- suppressWarnings(calc_burden(injd))
  expect_data_frame(df_burden, nrow = 1L, ncol = 6L)
  expect_subset(names(df_burden), c("totalexpo", "ndayslost", "burden",
                                    "burden_sd", "burden_lower", "burden_upper"))
  expect_identical(data.frame(round(df_burden, 3)), data.frame(totalexpo = 140950,
                                                               ndayslost = 3420,
                                                               burden = 218.375,
                                                               burden_sd = 3.734,
                                                               burden_lower = 211.057,
                                                               burden_upper = 225.694))

  df_burden <- calc_burden(injd, overall = FALSE, quiet = T)
  expect_data_frame(df_burden, ncol = 7L, nrow = 53L)
  expect_subset(c("person_id"), names(df_burden))

  df_burden <- calc_burden(injd, by = "injury_type", quiet = T)
  expect_data_frame(df_burden, ncol = 7L, nrow = 7L)
  expect_subset(c("injury_type"), names(df_burden))

  df_burden <- calc_burden(injd, by = "injury_type", overall = FALSE, quiet = T)
  expect_data_frame(df_burden, ncol = 8L, nrow = 371L)
  expect_subset(c("person_id", "injury_type"), names(df_burden))
})
