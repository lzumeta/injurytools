% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{date_format}
\alias{date_format}
\title{Proper Conversion of Date objects}
\usage{
date_format(date)
}
\arguments{
\item{date}{a \link[base:Dates]{Date} object.}
}
\value{
A "\%Y-\%m-\%d" formatted \link[base:Dates]{Date} object with respect to
a fixed locale and time zone, i.e. setting the LC_TIME component to C and
timezone to UTC (the safest choice, non-geographic and Coordinated
Universal Time).
}
\description{
Converts \link[base:Dates]{Date} objects into a common format used for every
Date object throughout the package.
}
\details{
To limit the scope of the changes to LC_TIME and the timezone, a temporary
locale modification is made using the \code{withr} package.
}
\keyword{internal}
