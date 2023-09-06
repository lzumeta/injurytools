test_that("check_injfollowup works", {
  followup_df <- attr(injd, "follow_up")
  data_injuries  <- prepare_inj(df_injuries0   = raw_df_injuries,
                                player         = "player_name",
                                date_injured   = "from",
                                date_recovered = "until")

  data_injuries$date_injured[[1]] <- as.Date("2014-01-09")
  expect_warning(check_injfollowup(followup_df, data_injuries), "has been cut to")

  data_injuries$date_injured[[1]] <- as.Date("2022-01-09")
  expect_warning(check_injfollowup(followup_df, data_injuries), "has been cut to")
})

test_that("new_injd works", {
  newinjd <- new_injd(data.frame(injd), "matches_minutes",
                   attr(injd, "follow_up"),
                   attr(injd, "data_exposures"),
                   attr(injd, "data_injuries"))
  expect_equal(is_injd(newinjd), TRUE)

  expect_error(new_injd(data.frame(injd), "match_min",
                        attr(injd, "follow_up"),
                        attr(injd, "data_exposures"),
                        attr(injd, "data_injuries")))
})

test_that("validate_injd works", {
  newinjd <- validate_injd(injd)
  expect_equal(newinjd, injd)

  follow_up <- attr(injd, "follow_up")
  follow_up[1, "t0"] <- as.Date("2017-07-05")
  injd2 <- injd
  attr(injd2, "follow_up") <- follow_up
  expect_error(validate_injd(injd2), "is not properly built")

  data_injuries <- attr(injd, "data_injuries")
  data_injuries <- data_injuries[-1, ]
  injd3 <- injd
  attr(injd3, "data_injuries") <- data_injuries
  expect_error(validate_injd(injd3), "do not have same info on injuries")

  injd4 <- injd
  injd4$status[[1]] <- -1
  expect_error(validate_injd(injd4), "status is not correctly recorded")

  injd5 <- injd
  injd5$tstop_minPlay[[2]] <- injd5$tstop_minPlay[[2]] + 1
  expect_error(validate_injd(injd5), "tstart and tstop are not correctly recorded")
})
