% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_data.R
\name{prepare_data}
\alias{prepare_data}
\alias{prepare_inj}
\alias{prepare_exp}
\alias{prepare_all}
\title{Prepare data in a standardized format}
\usage{
prepare_inj(
  df_injuries0,
  person_id = "person_id",
  date_injured = "date_injured",
  date_recovered = "date_recovered"
)

prepare_exp(
  df_exposures0,
  person_id = "person_id",
  date = "date",
  time_expo = "time_expo"
)

prepare_all(
  data_exposures,
  data_injuries,
  exp_unit = c("minutes", "hours", "days", "matches_num", "matches_minutes",
    "activity_days", "seasons")
)
}
\arguments{
\item{df_injuries0}{A data frame containing injury or illness information,
with columns referring to the athlete name/id, date of injury/illness and
date of recovery (as minimal data).}

\item{person_id}{Character referring to the column name storing sportsperson
(player, athlete) identification information.}

\item{date_injured}{Character referring to the column name where the
information about the date of injury or illness is stored.}

\item{date_recovered}{Character referring to the column name where the
information about the date of recovery is stored.}

\item{df_exposures0}{A data frame containing exposure information, with
columns referring to the sportsperson's name/id, date of exposure and the
total time of exposure of the corresponding data entry (as minimal data).}

\item{date}{Character referring to the column name where the exposure date
information is stored. Besides, the column must be of class
\link[base:Dates]{Date} or
\link[base:integer]{integer}/\link[base:numeric]{numeric}. If it is
\code{integer}/\code{numeric}, it should refer to the year in which the
season started (e.g. \code{date} = 2015 to refer to the 2015/2016 season).}

\item{time_expo}{Character referring to the column name where the information
about the time of exposure in that corresponding date is stored.}

\item{data_exposures}{Exposure data frame with standardized column names, in
the same fashion that \code{prepare_exp()} returns.}

\item{data_injuries}{Injury data frame with standardized column names, in the
same fashion that \code{prepare_inj()} returns.}

\item{exp_unit}{Character defining the unit of exposure time ("minutes" the
default).}
}
\value{
\code{prepare_inj()} returns a data frame in which the \strong{key
columns} in injury/illness data are standardized and have a proper format.

\code{prepare_exp()} returns a data frame in which the \strong{key
columns} in exposure data are standardized and have a proper format.

\code{prepare_all()} returns the \code{injd} \strong{S3} object that
contains all the necessary information and a proper data structure to
perform further statistical analyses (e.g. calculate injury summary
statistics, visualize injury data).
\itemize{
\item If \code{exp_unit} is "minutes" (the default), the columns
\code{tstart_min} and \code{tstop_min} are created which specify the time
to event (injury) values, the starting and stopping time of the interval,
respectively. That is the training time in minutes, that the sportsperson has
been at risk, until an injury/illness (or censorship) has occurred. For other
choices, \code{tstart_x} and \code{tstop_x} are also created according to
the \code{exp_unit} indicated (\code{x}, one of: \code{min}, \code{h},
\code{match}, \code{minPlay}, \code{d}, \code{acd} or \code{s}). These
columns will be useful for survival analysis routines. See Note section.
\item It also creates \code{days_lost} column based on the difference between
\code{date_recovered} and \code{date_injured} in days. And if it does exist (in
the raw data) it overrides.
}
}
\description{
These are the data preprocessing functions provided by the \code{injurytools}
package, which involve:
\enumerate{
\item setting \strong{exposure} and \strong{injury and illness data} in a standardized
format and
\item integrating both sources of data into an adequate data structure.
}
\code{prepare_inj()} and \code{prepare_exp()} set standardized names and
proper classes to the (key) columns in injury/illness and exposure data,
respectively. \code{prepare_all()} integrates both, standardized injury and
exposure data sets, and convert them into an \code{injd} \strong{S3} object
that has an adequate structure for further statistical analyses. See the
\href{https://lzumeta.github.io/injurytools/articles/prepare-injury-data.html}{Prepare
Sports Injury Data} vignette for details.
}
\note{
Depending on the unit of exposure, \code{tstart_x} and \code{tstop_x}
columns might have same values (e.g. if \code{exp_unit} = "matches_num" and the
player has not played any match between the corresponding period of time).
Please be aware of this before performing any survival analysis related
task.
}
\examples{
df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                           person_id      = "player_name",
                           date_injured   = "from",
                           date_recovered = "until")

df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                            person_id     = "player_name",
                            date          = "year",
                            time_expo     = "minutes_played")

\donttest{
injd <- prepare_all(data_exposures = df_exposures,
                    data_injuries  = df_injuries,
                    exp_unit = "matches_minutes")
head(injd)
class(injd)
str(injd, 1)
}
}
