
test_that("gg_injprev_polar works", {
  gg1 <- gg_injprev_polar(injd, by = "monthly", var_type_injury = "injury_type",
                   title = NULL)
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- gg_injprev_polar(injd, by = "monthly",
                   title = "Polar area diagram\ninjured and available (healthy) players per month")
  expect_s3_class(gg2, class = c("gg", "ggplot"))

  gg3 <- gg_injprev_polar(injd, by = "season", var_type_injury = "injury_type",
                          title = "Polar area diagram\ninjured and available (healthy) players per month")
  expect_s3_class(gg3, class = c("gg", "ggplot"))

  gg4 <- gg_injprev_polar(injd, by = "season",
                          title = "Polar area diagram\ninjured and available (healthy) players per month")
  expect_s3_class(gg4, class = c("gg", "ggplot"))
})
