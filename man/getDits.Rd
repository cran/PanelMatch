% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation_components.R
\name{getDits}
\alias{getDits}
\title{getDits
returns a vector of Dit values, as defined in the paper. They should be in the same order as the data frame containing the original problem data.}
\usage{
getDits(matched_sets, data)
}
\arguments{
\item{matched_sets}{matched.set object}

\item{data}{data.frame object}
}
\value{
vector of Dits, as described in Imai et al. (2023)
}
\description{
getDits
returns a vector of Dit values, as defined in the paper. They should be in the same order as the data frame containing the original problem data.
}
\keyword{internal}
