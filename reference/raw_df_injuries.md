# Minimal example of injury data

An example of an injury data set containing minimum required injury
information as well as other further injury-related variables. It
includes Liverpool Football Club male's first team players' injury data.
Each row refers to player-injury. These data have been scrapped from
<https://www.transfermarkt.com/> website using self-defined **R** code
with `rvest` and `xml2` packages.

## Usage

``` r
raw_df_injuries
```

## Format

A data frame with 82 rows corresponding to 23 players and 11 variables:

- player_name:

  Name of the football player (factor)

- player_id:

  Identification number of the football player (factor)

- season:

  Season to which this player's entry corresponds (factor)

- from:

  Date of the injury of each data entry (Date)

- until:

  Date of the recovery of each data entry (Date)

- days_lost:

  Number of days lost due to injury (numeric)

- games_lost:

  Number of matches lost due to injury (numeric)

- injury:

  Injury specification as it appears in <https://www.transfermarkt.com>
  (character)

- injury_acl:

  Whether it is Anterior Cruciate Ligament (ACL) injury or not (NO_ACL)

- injury_type:

  A five level categorical variable indicating the type of injury,
  whether Bone, Concussion, Ligament, Muscle or Unknown; if any, NA
  otherwise (factor)

- injury_severity:

  A four level categorical variable indicating the severity of the
  injury (if any), whether Minor (\<7 days lost), Moderate (\[7, 28)
  days lost), Severe (\[28, 84) days lost) or Very_severe (\>=84 days
  lost); NA otherwise (factor)

## Source

<https://www.transfermarkt.com/>

## Note

This data frame is provided for illustrative purposes. We warn that they
might not be accurate, there might be a mismatch and non-completeness
with what actually occurred. As such, its use cannot be recommended for
epidemiological research (see also Hoenig et al., 2022).

## References

Hoenig, T., Edouard, P., Krause, M., Malhan, D., Relógio, A., Junge, A.,
& Hollander, K. (2022). Analysis of more than 20,000 injuries in
European professional football by using a citizen science-based
approach: An opportunity for epidemiological research?. *Journal of
science and medicine in sport*, 25(4), 300-305.
