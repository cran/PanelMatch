% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation_components.R
\name{pcs}
\alias{pcs}
\title{Prepare Control Units
pcs and pts create data frames with the time/id combinations--that need to be found so that they can be easily looked up in the data frame via a hash table. The data frame also contains information about the weight of that unit at particular times, so we use the hash table to look up where to put this data so that we can easily assign the appropriate weights in the original data frame containing the problem data. pcs does this for all control units in a matched set. pts does this for all treated units.}
\usage{
pcs(sets, lead.in)
}
\arguments{
\item{sets}{object describing the matched sets}

\item{lead.in}{integer describing a particular lead value.}
}
\value{
data.frame object with time-id combinations
}
\description{
Prepare Control Units
pcs and pts create data frames with the time/id combinations--that need to be found so that they can be easily looked up in the data frame via a hash table. The data frame also contains information about the weight of that unit at particular times, so we use the hash table to look up where to put this data so that we can easily assign the appropriate weights in the original data frame containing the problem data. pcs does this for all control units in a matched set. pts does this for all treated units.
}
\keyword{internal}
