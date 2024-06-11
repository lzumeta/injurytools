

test_that("gg_rank works", {
  gg1 <- gg_rank(injd, summary_stat = "incidence",
                       title = "Overall injury incidence per player")
  expect_s3_class(gg1, class = c("gg", "ggplot"))

  gg2 <- gg_rank(injd, summary_stat = "burden",
                       title = NULL)
  expect_s3_class(gg2, class = c("gg", "ggplot"))

  gg3 <- gg_rank(injd)
  expect_error(gg_rank(injd, summary_stat = NULL))

  gg4 <- gg_rank(injd, summary_stat = "burden",
                 line_overall = TRUE,
                 title = NULL)
  expect_s3_class(gg4, class = c("gg", "ggplot"))
})
