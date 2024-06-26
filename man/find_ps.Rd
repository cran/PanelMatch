% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propensity_score_utilities.R
\name{find_ps}
\alias{find_ps}
\title{find_ps}
\usage{
find_ps(sets, fitted.model)
}
\arguments{
\item{sets}{matched sets}

\item{fitted.model}{Result of a fitted (CB) PS model call}
}
\value{
Returns a list of data frames with propensity score weights for each unit in a matched set. Each element in the list is a data frame which corresponds to a matched set of 1 treatment and all matched control units
}
\description{
find_ps
}
\keyword{internal}
