% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matched_set_helpers.r
\name{findBinaryTreated}
\alias{findBinaryTreated}
\title{findBinaryTreated}
\usage{
findBinaryTreated(
  dmat,
  qoi.in,
  treatedvar,
  time.var,
  unit.var,
  hasbeensorted = FALSE
)
}
\arguments{
\item{dmat}{Data frame or matrix containing data used to identify potential treated units. Must be specified in such a way that a combination of time and id variables will correspond to a unique row. Must also contain at least a binary treatment variable column as well.}

\item{treatedvar}{Character string that identifies the name of the column in \code{dmat} that provides information about the binary treatment variable}

\item{time.var}{Character string that identifies the name of the column in \code{dmat} that contains data about the time variable. This data must be integer that increases by one.}

\item{unit.var}{Character string that identifies the name of the column in \code{dmat} that contains data about the variable used as a unit id. This data must be integer}

\item{hasbeensorted}{variable that only has internal usage for optimization purposes. There should be no need for a user to toggle this}
}
\value{
\code{findBinaryTreated} returns a subset of the data in the \code{dmat} data frame, containing only treated units for which a matched set might exist
}
\description{
\code{findBinaryTreated} is used to identify t,id pairs of units for which a matched set might exist.
More precisely, it finds units for which at time t, the specified treatment has been applied, but at time t - 1, the treatment has not.
}
\keyword{internal}
