

test_that("gg_injbarplot works", {
  injds <- suppressWarnings(injsummary(injd))
  gg1 <- gg_injbarplot(injds, type = "incidence",
                      title = "Overall injury incidence per player")
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- gg_injbarplot(injds, type = "burden",
                       title = NULL)
  expect_s3_class(gg2, class = c("gg", "ggplot"))

  gg3 <- gg_injbarplot(injds)
  expect_error(gg_injbarplot(injds, type = NULL))
})
