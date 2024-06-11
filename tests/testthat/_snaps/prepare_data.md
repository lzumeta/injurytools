# prepare_all works fine when dates in injury and exposure data do not match

    Code
      out <- prepare_all(data_exposures = data_exposures, data_injuries = df_injuries,
        exp_unit = "matches_minutes")
    Condition
      Warning in `check_injfollowup()`:
      Injury/illness data has been cut to the given follow-up period (in exposure data)

