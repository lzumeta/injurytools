
test_that("injprev works", {
  injprev_df <- injprev(injd, by = "monthly", var_type_injury = "injury_type")
  expect_data_frame(injprev_df, nrows = 132L, ncols = 6L)
  expect_subset(c("season", "month", "type_injury", "n", "n_player", "prop"),
                names(injprev_df))
  expect_equal(levels(injprev_df$month),
               month.abb[c(7:12, 1:6)])
  expect_equal(levels(injd$injury_type),
               levels(injprev_df$type_injury)[-1])
  expect_equal("Available",
               levels(injprev_df$type_injury)[[1]])

  injprev_df <- injprev(injd, by = "monthly")
  expect_data_frame(injprev_df, nrows = 48L, ncols = 6L)

  injprev_df <- injprev(injd, by = "season", var_type_injury = "injury_type")
  expect_data_frame(injprev_df, nrows = 16L, ncols = 5L)
  expect_subset(c("season", "type_injury", "n", "n_player", "prop"),
                names(injprev_df))

  injprev_df <- injprev(injd, by = "season")
  expect_data_frame(injprev_df, nrows = 4L, ncols = 5L)
})
