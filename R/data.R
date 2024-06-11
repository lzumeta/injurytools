#' Minimal example of exposure data
#'
#' An example of a player exposure data set that contains minimum required
#' exposure information as well as other player- and match-related variables. It
#' includes Liverpool Football Club male's first team players' exposure data,
#' exposure measured as (number or minutes of) matches played, over two
#' consecutive seasons, 2017-2018 and 2018-2019. Each row refers to
#' player-season. These data have been scrapped from
#' \url{https://www.transfermarkt.com/} website using self-defined **R** code
#' with `rvest` and `xml2` packages.
#'
#' @note This data frame is provided for illustrative purposes. We warn that
#'   they might not be accurate, there might be a mismatch and non-completeness
#'   with what actually occurred. As such, its use cannot be recommended for
#'   epidemiological research (see also Hoenig et al., 2022).
#'
#' @references Hoenig, T., Edouard, P., Krause, M., Malhan, D., Relógio, A.,
#'   Junge, A., & Hollander, K. (2022). Analysis of more than 20,000 injuries in
#'   European professional football by using a citizen science-based approach:
#'   An opportunity for epidemiological research?. \emph{Journal of science and
#'   medicine in sport}, 25(4), 300-305.
#'
#' @format A data frame with 42 rows corresponding to 28 football players and 16
#'   variables: \describe{
#'   \item{player_name}{Name of the football player (factor)}
#'   \item{player_id}{Identification number of the football player (factor)}
#'   \item{season}{Season to which this player's entry corresponds (factor)}
#'   \item{year}{Year in which each season started (numeric)}
#'   \item{matches_played}{Matches played by the player in each season (numeric)}
#'   \item{minutes_played}{Minutes played by the player in each season (numeric)}
#'   \item{liga}{Name of the ligue where the player played in each season (factor)}
#'   \item{club_name}{Name of the club to which the player belongs in each season (factor)}
#'   \item{club_id}{Identification number of the club to which the player belongs in each season (factor)}
#'   \item{age}{Age of the player in each season (numeric)}
#'   \item{height}{Height of the player in m (numeric)}
#'   \item{place}{Place of birth of each player (character)}
#'   \item{citizenship}{Citizenship of the player (factor)}
#'   \item{position}{Position of the player on the pitch (factor)}
#'   \item{foot}{Dominant leg of the player. One of both, left or right (factor)}
#'   \item{goals}{Number of goals scored by the player in that season (numeric)}
#'   \item{assists}{Number of assists provided by the player in that season (numerical)}
#'   \item{yellows}{Number of the yellow cards received by the player in that season (numeric)}
#'   \item{reds}{Number of the red cards received by the player in that season (numeric)}}
#' @source \url{https://www.transfermarkt.com/}
"raw_df_exposures"


#' Minimal example of injury data
#'
#' An example of an injury data set containing minimum required injury
#' information as well as other further injury-related variables. It includes
#' Liverpool Football Club male's first team players' injury data. Each row
#' refers to player-injury. These data have been scrapped from
#' \url{https://www.transfermarkt.com/} website using self-defined **R** code
#' with `rvest` and `xml2` packages.
#'
#' @note This data frame is provided for illustrative purposes. We warn that
#'   they might not be accurate, there might be a mismatch and non-completeness
#'   with what actually occurred. As such, its use cannot be recommended for
#'   epidemiological research (see also Hoenig et al., 2022).
#'
#' @references Hoenig, T., Edouard, P., Krause, M., Malhan, D., Relógio, A.,
#'   Junge, A., & Hollander, K. (2022). Analysis of more than 20,000 injuries in
#'   European professional football by using a citizen science-based approach:
#'   An opportunity for epidemiological research?. \emph{Journal of science and
#'   medicine in sport}, 25(4), 300-305.
#'
#' @format A data frame with 82 rows corresponding to 23 players and 11
#'   variables: \describe{
#'   \item{player_name}{Name of the football player (factor)}
#'   \item{player_id}{Identification number of the football player (factor)}
#'   \item{season}{Season to which this player's entry corresponds (factor)}
#'   \item{from}{Date of the injury of each data entry (Date)}
#'   \item{until}{Date of the recovery of each data entry (Date)}
#'   \item{days_lost}{Number of days lost due to injury (numeric)}
#'   \item{games_lost}{Number of matches lost due to injury (numeric)}
#'   \item{injury}{Injury specification as it appears in \url{https://www.transfermarkt.com} (character)}
#'   \item{injury_acl}{Whether it is Anterior Cruciate Ligament (ACL) injury or not (NO_ACL)}
#'   \item{injury_type}{A five level categorical variable indicating the type of
#'   injury, whether Bone, Concussion, Ligament, Muscle or Unknown; if any, NA
#'   otherwise (factor)}
#'   \item{injury_severity}{A four level categorical variable indicating the
#'   severity of the injury (if any), whether Minor (<7 days lost), Moderate
#'   ([7, 28) days lost), Severe ([28, 84) days lost) or
#'   Very_severe (>=84 days lost); NA otherwise (factor)} }
#' @source \url{https://www.transfermarkt.com/}
"raw_df_injuries"

#' Example of an `injd` object
#'
#' An \code{injd} object (\strong{S3}), called \code{injd}, to showcase what
#' this object is like and also to save computation time in some help files
#' provided by the package. The result of applying \code{prepare_all()} to
#' *raw_df_exposures* (\code{prepare_exp(raw_df_exposures, ...)}) and \cr
#' *raw_df_injuries* (\code{prepare_inj(raw_df_injuries, ...)}).
#'
#' It consists of a data frame plus 4 other attributes:
#' a character specifying the unit of exposure (\code{unit_exposure}); and 3
#' (auxiliary) data frames: \code{follow_up}, \code{data_exposures} and
#' \code{data_injuries}.
#'
#' @format The main data frame in \code{injd} gathers information of 28 players
#'   and has 108 rows and 19 columns:
#'  \describe{
#'    \item{person_id}{Player identifier (factor)}
#'    \item{t0}{Follow-up period of the corresponding player, i.e. player’s first observed date, same value for each player (Date)}
#'    \item{tf}{Follow-up period of the corresponding player, i.e. player’s last observed date, same value for each player (Date)}
#'    \item{date_injured}{Date of injury of the corresponding observation (if any). Otherwise NA (Date)}
#'    \item{date_recovered}{Date of recovery of the corresponding observation (if any). Otherwise NA (Date)}
#'    \item{tstart}{Beginning date of the corresponding interval in which the observation has been at risk of injury (Date)}
#'    \item{tstop}{Ending date of the corresponding interval in which the observation has been at risk of injury (Date)}
#'    \item{tstart_minPlay}{Beginning time. Minutes played in matches until the start of this interval in which the observation has been at risk of injury (numeric)}
#'    \item{tstop_minPlay}{Ending time. Minutes played in matches until the finish of this interval in which the observation has been at risk of injury (numeric)}
#'    \item{status}{injury (event) indicator (numeric)}
#'    \item{enum}{an integer indicating the recurrence number, i.e. the \eqn{k}-th injury (event), at which the observation is at risk}
#'    \item{days_lost}{Number of days lost due to injury (numeric)}
#'    \item{player_id}{Identification number of the football player (factor)}
#'    \item{season}{Season to which this player's entry corresponds (factor)}
#'    \item{games_lost}{Number of matches lost due to injury (numeric)}
#'    \item{injury}{Injury specification as it appears in \url{https://www.transfermarkt.com}, if any; otherwise NA (character)}
#'    \item{injury_acl}{Whether it is Anterior Cruciate Ligament (ACL) injury or not (NO_ACL); if the interval corresponds to an injury, NA otherwise (factor)}
#'    \item{injury_type}{A five level categorical variable indicating the type
#'     of injury, whether Bone, Concussion, Ligament, Muscle or Unknown; if any,
#'     NA otherwise (factor)}
#'    \item{injury_severity}{A four level categorical variable indicating the
#'     severity of the injury (if any), whether Minor (<7 days lost), Moderate ([7, 28)
#'     days lost), Severe ([28, 84) days lost) or Very_severe (>=84 days lost);
#'     NA otherwise (factor)}
#'   }
"injd"
