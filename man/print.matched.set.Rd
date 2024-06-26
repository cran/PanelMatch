% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matched_set_obj.R
\name{print.matched.set}
\alias{print.matched.set}
\title{Print \code{matched.set} objects.}
\usage{
\method{print}{matched.set}(x, ..., verbose = FALSE)
}
\arguments{
\item{x}{a \code{matched.set} object}

\item{...}{additional arguments to be passed to \code{print}}

\item{verbose}{logical indicating whether or not output should be printed in expanded/raw list form.
The verbose form is not recommended unless the data set is small. Default is FALSE}
}
\description{
Print \code{matched.set} objects.
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
print(PM.results$att)



}
