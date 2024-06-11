test_that("calc_incidence works", {
  df_incidence <- suppressWarnings(calc_incidence(injd))
  expect_data_frame(df_incidence, nrow = 1L, ncol = 6L)
  expect_subset(names(df_incidence), c("totalexpo", "ncases", "incidence",
                                       "incidence_sd", "incidence_lower", "incidence_upper"))
  expect_identical(data.frame(round(df_incidence, 3)), data.frame(
    totalexpo = 140950,
    ncases = 209,
    incidence = 13.345,
    incidence_sd = 0.923,
    incidence_lower = 11.536,
    incidence_upper = 15.154
  ))

  df_incidence <- calc_incidence(injd, overall = FALSE, quiet = T)
  expect_data_frame(df_incidence, ncol = 7L, nrow = 53L)
  expect_subset(c("person_id"), names(df_incidence))

  df_incidence <- calc_incidence(injd, by = "injury_type", quiet = T)
  expect_data_frame(df_incidence, ncol = 7L, nrow = 7L)
  expect_subset(c("injury_type"), names(df_incidence))

  df_incidence <- calc_incidence(injd, by = "injury_type", overall = FALSE, quiet = T)
  expect_data_frame(df_incidence, ncol = 8L, nrow = 371L)
  expect_subset(c("person_id", "injury_type"), names(df_incidence))
})
