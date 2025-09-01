test_that("check_injfollowup works", {
  followup_df <- get_data_followup(injd)
  data_injuries  <- prepare_inj(df_injuries0   = raw_df_injuries,
                                person_id      = "player_name",
                                date_injured   = "from",
                                date_recovered = "until")

  data_injuries$date_injured[[1]] <- as.Date("2014-01-09")
  expect_warning(check_injfollowup(followup_df, data_injuries), "has been cut to")

  data_injuries$date_injured[[1]] <- as.Date("2022-01-09")
  expect_warning(check_injfollowup(followup_df, data_injuries), "has been cut to")
})

test_that("new_injd works", {
  newinjd <- new_injd(data.frame(injd), "matches_minutes")
  expect_equal(is_injd(newinjd), TRUE)

  expect_error(new_injd(data.frame(injd), "match_min"))
})

test_that("validate_injd works", {
  newinjd <- validate_injd(injd)
  expect_equal(newinjd, injd)

  injd5 <- injd
  injd5$tstop_minPlay[[2]] <- injd5$tstop_minPlay[[2]] + 1
  expect_error(validate_injd(injd5), "tstart and tstop are not correctly recorded")

  injdquitvar <- injd
  names(injdquitvar)[[3]] <- "tfinal"
  expect_error(validate_injd(injdquitvar), "The `x` data frame has not proper column names")

  injddates <- injd
  injddates$date_injured <- injddates$date_recovered + 1
  expect_error(validate_injd(injddates), "Injury and recovery dates are not properly recorded in `x`")

  injddates <- injd
  injddates$date_injured[[1]] <- NA
  injddates$status[[1]] <- 1
  expect_error(validate_injd(injddates), "status is not correctly recorded in `x`")
})
