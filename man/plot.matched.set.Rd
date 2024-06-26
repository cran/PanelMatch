% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matched_set_obj.R
\name{plot.matched.set}
\alias{plot.matched.set}
\title{Plot the distribution of the sizes of matched sets.}
\usage{
\method{plot}{matched.set}(
  x,
  ...,
  border = NA,
  col = "grey",
  ylab = "Frequency of Size",
  xlab = "Matched Set Size",
  lwd = NULL,
  main = "Distribution of Matched Set Sizes",
  freq = TRUE,
  include.empty.sets = FALSE
)
}
\arguments{
\item{x}{a \code{matched.set} object}

\item{...}{optional arguments to be passed to \code{hist()}}

\item{border}{default is NA. This is the same argument as the standard argument for \code{hist()}}

\item{col}{default is "grey". This is the same argument as the standard argument for \code{hist()}}

\item{ylab}{default is "Frequency of Size". This is the same argument as the standard argument for \code{hist()}}

\item{xlab}{default is "Matched Set Size". This is the same argument as the standard argument for \code{hist()}}

\item{lwd}{default is NULL. This is the same argument as the standard argument for \code{hist()}}

\item{main}{default is "Distribution of Matched Set Sizes". This is the same argument as the standard argument for \code{hist}}

\item{freq}{default is TRUE. See \code{freq} argument in \code{hist()} function for more.}

\item{include.empty.sets}{logical value indicating whether or not empty sets should be included in the histogram. default is FALSE. If FALSE, then empty sets will be noted as a separate vertical bar at x = 0. If TRUE, empty sets will be included as normal sets.}
}
\description{
A plot method for creating a histogram of the distribution of the sizes of matched sets.
This method accepts all standard optional \code{hist} arguments via the \code{...} argument.
By default, empty matched sets (treated units that could not be
matched with any control units) are noted as a vertical bar at x = 0 and not included in the
regular histogram. See the \code{include.empty.sets} argument for more information about this.
}
\examples{
dem.sub <- dem[dem[, "wbcode2"] <= 100, ]
# create subset of data for simplicity
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "ps.match",
                         data = dem, match.missing = TRUE,
                         covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                         size.match = 5, qoi = "att",
                         outcome.var = "y", lead = 0:4, forbid.treatment.reversal = FALSE)
plot(PM.results$att)
plot(PM.results$att, include.empty.sets = TRUE)

}
