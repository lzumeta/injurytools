# Minimal example of exposure data

An example of a player exposure data set that contains minimum required
exposure information as well as other player- and match-related
variables. It includes Liverpool Football Club male's first team
players' exposure data, exposure measured as (number or minutes of)
matches played, over two consecutive seasons, 2017-2018 and 2018-2019.
Each row refers to player-season. These data have been scrapped from
<https://www.transfermarkt.com/> website using self-defined **R** code
with `rvest` and `xml2` packages.

## Usage

``` r
raw_df_exposures
```

## Format

A data frame with 42 rows corresponding to 28 football players and 16
variables:

- player_name:

  Name of the football player (factor)

- player_id:

  Identification number of the football player (factor)

- season:

  Season to which this player's entry corresponds (factor)

- year:

  Year in which each season started (numeric)

- matches_played:

  Matches played by the player in each season (numeric)

- minutes_played:

  Minutes played by the player in each season (numeric)

- liga:

  Name of the ligue where the player played in each season (factor)

- club_name:

  Name of the club to which the player belongs in each season (factor)

- club_id:

  Identification number of the club to which the player belongs in each
  season (factor)

- age:

  Age of the player in each season (numeric)

- height:

  Height of the player in m (numeric)

- place:

  Place of birth of each player (character)

- citizenship:

  Citizenship of the player (factor)

- position:

  Position of the player on the pitch (factor)

- foot:

  Dominant leg of the player. One of both, left or right (factor)

- goals:

  Number of goals scored by the player in that season (numeric)

- assists:

  Number of assists provided by the player in that season (numerical)

- yellows:

  Number of the yellow cards received by the player in that season
  (numeric)

- reds:

  Number of the red cards received by the player in that season
  (numeric)

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
