
test_that("gg_photo works", {
  gg1 <- gg_photo(injd, title = "Injury Overview", by_date = "1 years")
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- gg_photo(injd, title = NULL, by_date = "1 years")
  expect_s3_class(gg2, class = c("gg", "ggplot"))

  gg3 <- gg_photo(injd, title = NULL, by_date = "3 months", fix = TRUE)
  expect_s3_class(gg3, class = c("gg", "ggplot"))

  expect_error(gg_photo(injd, title = NULL, by_date = NULL))
})
