
test_that("gg_injriskmatrix works", {
  injds2 <- injsummary(injd, var_type_injury = "injury_type", quiet = T)
  gg1 <- suppressWarnings(
    gg_injriskmatrix(injds2, var_type_injury = "injury_type", title = "Risk matrix",
                     add_contour = FALSE)
  )
  expect_s3_class(gg1, class = c("gg","ggplot"))

  gg2 <- suppressWarnings(
    gg_injriskmatrix(injds2, var_type_injury = "injury_type", add_contour = TRUE)
    )
  expect_s3_class(gg2, class = c("gg","ggplot"))
})

test_that("gg_injriskmatrix throws warnings", {
  injds2 <- injsummary(injd, var_type_injury = "injury_type", quiet = T)

  expect_warning(gg_injriskmatrix(injds2, var_type_injury = "injury_type", title = "Risk matrix"),
                 "An injury average time-loss")
})
