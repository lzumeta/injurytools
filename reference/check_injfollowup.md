# Check the dates in injury data and exposure data

Check that dates of injury data are within the dates of exposure data.

## Usage

``` r
check_injfollowup(followup_df, data_injuries)
```

## Arguments

- followup_df:

  Data frame created inside prepare_all() function.

- data_injuries:

  Data frame given in prepare_all() function.

## Value

Same injury data or cut injury data in which the dates of injury
occurrences are within the dates of each person_id's follow-up period.
