# Example of an `injd` object

An `injd` object (**S3**), called `injd`, to showcase what this object
is like and also to save computation time in some help files provided by
the package. The result of applying
[`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)
to *raw_df_exposures* (`prepare_exp(raw_df_exposures, ...)`) and  
*raw_df_injuries* (`prepare_inj(raw_df_injuries, ...)`).

## Usage

``` r
injd
```

## Format

The main data frame in `injd` gathers information of 28 players and has
108 rows and 19 columns:

- person_id:

  Player identifier (factor)

- t0:

  Follow-up period of the corresponding player, i.e. player’s first
  observed date, same value for each player (Date)

- tf:

  Follow-up period of the corresponding player, i.e. player’s last
  observed date, same value for each player (Date)

- date_injured:

  Date of injury of the corresponding observation (if any). Otherwise NA
  (Date)

- date_recovered:

  Date of recovery of the corresponding observation (if any). Otherwise
  NA (Date)

- tstart:

  Beginning date of the corresponding interval in which the observation
  has been at risk of injury (Date)

- tstop:

  Ending date of the corresponding interval in which the observation has
  been at risk of injury (Date)

- tstart_minPlay:

  Beginning time. Minutes played in matches until the start of this
  interval in which the observation has been at risk of injury (numeric)

- tstop_minPlay:

  Ending time. Minutes played in matches until the finish of this
  interval in which the observation has been at risk of injury (numeric)

- status:

  injury (event) indicator (numeric)

- enum:

  an integer indicating the recurrence number, i.e. the \\k\\-th injury
  (event), at which the observation is at risk

- days_lost:

  Number of days lost due to injury (numeric)

- player_id:

  Identification number of the football player (factor)

- season:

  Season to which this player's entry corresponds (factor)

- games_lost:

  Number of matches lost due to injury (numeric)

- injury:

  Injury specification as it appears in <https://www.transfermarkt.com>,
  if any; otherwise NA (character)

- injury_acl:

  Whether it is Anterior Cruciate Ligament (ACL) injury or not (NO_ACL);
  if the interval corresponds to an injury, NA otherwise (factor)

- injury_type:

  A five level categorical variable indicating the type of injury,
  whether Bone, Concussion, Ligament, Muscle or Unknown; if any, NA
  otherwise (factor)

- injury_severity:

  A four level categorical variable indicating the severity of the
  injury (if any), whether Minor (\<7 days lost), Moderate (\[7, 28)
  days lost), Severe (\[28, 84) days lost) or Very_severe (\>=84 days
  lost); NA otherwise (factor)

## Details

It consists of a data frame plus 4 other attributes: a character
specifying the unit of exposure (`unit_exposure`); and 3 (auxiliary)
data frames: `follow_up`, `data_exposures` and `data_injuries`.
