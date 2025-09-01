test_that("summary_unit works", {
  summary_data <- calc_summary(injd, quiet = TRUE)
  res <- summary_unit("hours", summary_data, quiet = TRUE)
  expected_res <- tibble(
    totalexpo = 140950,
    ncases = 209,
    incidence = 13345.16,
    incidence_sd = 923.1039,
    incidence_lower = 11535.91,
    incidence_upper = 15154.41,
    ndayslost = 3420,
    mean_dayslost = 16.36364,
    median_dayslost = 7,
    qt25_dayslost = 4,
    qt75_dayslost = 16,
    burden = 218375.3,
    burden_sd = 3734.139,
    burden_lower = 211056.5,
    burden_upper = 225694.1
  )
  expect_equal(res, expected_res, tolerance = 0.001)

  res <- summary_unit("minutes", summary_data, quiet = TRUE)
  expected_res <- tibble(
    totalexpo = 140950,
    ncases = 209,
    incidence = 800709.5,
    incidence_sd = 55386.23,
    incidence_lower = 692154.5,
    incidence_upper = 909264.5,
    ndayslost = 3420,
    mean_dayslost = 16.36364,
    median_dayslost = 7,
    qt25_dayslost = 4,
    qt75_dayslost = 16,
    burden = 13102519,
    burden_sd = 224048.3,
    burden_lower = 12663392,
    burden_upper = 13541645
  )
  expect_equal(res, expected_res, tolerance = 0.001)
  expect_warning(summary_unit("minutes", summary_data, quiet = FALSE),
                 "\n Exposure time unit is minutes")

  res <- summary_unit("matches_minutes", summary_data, quiet = TRUE)
  expected_res <- tibble(
    totalexpo = 140950,
    ncases = 209,
    incidence = 120106.4,
    incidence_sd = 8307.935,
    incidence_lower = 103823.2,
    incidence_upper = 136389.7,
    ndayslost = 3420,
    mean_dayslost = 16.36364,
    median_dayslost = 7,
    qt25_dayslost = 4,
    qt75_dayslost = 16,
    burden = 1965378,
    burden_sd = 33607.25,
    burden_lower = 1899509,
    burden_upper = 2031247
  )
  expect_equal(res, expected_res, tolerance = 0.001)
  expect_warning(summary_unit("matches_minutes", summary_data, quiet = FALSE),
                 "Exposure time unit is matches")

  res <- summary_unit("activity_days", summary_data, quiet = TRUE)
  expected_res <- tibble(
    totalexpo = 140950,
    ncases = 209,
    incidence = 1334.516,
    incidence_sd = 92.31039,
    incidence_lower = 1153.591,
    incidence_upper = 1515.441,
    ndayslost = 3420,
    mean_dayslost = 16.36364,
    median_dayslost = 7,
    qt25_dayslost = 4,
    qt75_dayslost = 16,
    burden = 21837.53,
    burden_sd = 373.4139,
    burden_lower = 21105.65,
    burden_upper = 22569.41
  )
  expect_equal(res, expected_res, tolerance = 0.001)
  expect_warning(summary_unit("days", summary_data, quiet = FALSE),
                 "Exposure time unit is days")
})
