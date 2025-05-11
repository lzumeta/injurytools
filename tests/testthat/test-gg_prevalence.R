test_that("gg_prevalence works", {
  gg1 <- gg_prevalence(injd, time_period = "monthly", by = "injury_type")
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- gg_prevalence(injd, time_period = "monthly",
                title = "Monthly prevalence of sports injuries")
  expect_s3_class(gg2, class = c("gg", "ggplot"))

  gg3 <- gg_prevalence(injd, time_period = "season")
  expect_s3_class(gg3, class = c("gg", "ggplot"))

  gg4 <- suppressWarnings(gg_prevalence(injd, time_period = "season", by = "injury_type"))
  expect_s3_class(gg4, class = c("gg", "ggplot"))

  gg5 <- suppressWarnings(gg_prevalence(injd, time_period = "season", by = "injury_type",
                                        line_mean = T))
  expect_s3_class(gg5, class = c("gg", "ggplot"))

  expect_warning(gg_prevalence(injd, time_period = "season", by = "injury_type",
                               line_mean = T), regexp = "because there are athletes")

  gg6 <- suppressWarnings(gg_prevalence(injd, time_period = "season",
                                        line_mean = T))
  expect_s3_class(gg6, class = c("gg", "ggplot"))

  gg7 <- suppressWarnings(gg_prevalence(injd, time_period = "monthly",
                                        line_mean = T))
  expect_s3_class(gg7, class = c("gg", "ggplot"))

  gg8 <- suppressWarnings(gg_prevalence(injd, time_period = "monthly", by = "injury_type",
                                        line_mean = T))
  expect_s3_class(gg8, class = c("gg", "ggplot"))
})

