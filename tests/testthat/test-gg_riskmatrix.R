
test_that("gg_riskmatrix works", {
  gg1 <- suppressWarnings(
    gg_riskmatrix(injd, by = "injury_type", title = "Risk matrix",
                add_contour = FALSE)
  )
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- suppressWarnings(
    gg_riskmatrix(injd, by = "injury_type", quiet = TRUE, add_contour = TRUE)
  )
  expect_s3_class(gg2, class = c("gg", "ggplot"))
})

test_that("gg_riskmatrix throws warnings", {
  expect_warning(gg_riskmatrix(injd, by = "injury_type",
                                  title = "Risk matrix"),
                 "An injury average time-loss")
})
