
test_that("gg_injphoto works", {
  gg1 <- gg_injphoto(injd, title = "Injury Overview", by_date = "1 years")
  expect_s3_class(gg1, class = c("gg","ggplot"))

  gg2 <- gg_injphoto(injd, title = NULL, by_date = "1 years")
  expect_s3_class(gg2, class = c("gg","ggplot"))

  gg3 <- gg_injphoto(injd, title = NULL, by_date = "3 months", fix = TRUE)
  expect_s3_class(gg3, class = c("gg","ggplot"))

  expect_error(gg_injphoto(injd, title = NULL, by_date = NULL))
})
