raw_df_injuries <- dplyr::filter(raw_df_injuries, player_name %in% c("lionel-messi", "mohamed-salah"))
raw_df_exposures <- dplyr::filter(raw_df_exposures, player_name %in% c("lionel-messi", "mohamed-salah"))

df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                           player         = "player_name",
                           date_injured   = "from",
                           date_recovered = "until")

df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                            player        = "player_name",
                            date          = "year",
                            time_expo     = "minutes_played")

injd <- prepare_all(data_exposures = df_exposures,
                    data_injuries  = df_injuries,
                    exp_unit = "matches_minutes")


injds <- injsummary(injd, quiet = TRUE)

test_that("Test that injsummary works", {
  expect_equal(is.list(injds), TRUE)
  expect_s3_class(injds, "injds")
  expect_equal(is_injds(injds), TRUE)
  expect_type(attr(injds, "unit_exposure"), "character")
  expect_type(attr(injds, "unit_timerisk"), "character")
  expect_subset(c("player", "ninjuries", "ndayslost", "totalexpo", "injincidence",
                  "injburden"),
                names(injds[[1]]))
  expect_subset(c("ninjuries", "ndayslost", "totalexpo", "injincidence",
                  "injburden", "injincidence_sd", "injburden_sd",
                  "injincidence_lower", "injincidence_upper",
                  "injburden_lower", "injburden_upper"),
                names(injds[[2]]))
})

test_that("Test that warnings are working", {
  w <- capture_warnings(injsummary(injd))
  expect_length(w, 2)
  expect_match(w, regexp = "Exposure time unit is")

  df_exposures[df_exposures$player == df_exposures$player[[1]], "time_expo"] <- 0
  injd <- prepare_all(data_exposures = df_exposures,
                      data_injuries  = df_injuries,
                      exp_unit = "matches_minutes")
  w <- capture_warnings(injsummary(injd))
  expect_length(w, 3)
  expect_match(w, regexp = "(There are players)|(Exposure time unit is)")
})
