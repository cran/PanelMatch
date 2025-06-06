% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PanelMatch.R
\name{PanelMatch}
\alias{PanelMatch}
\title{Create and refine sets of matched treated and control observations}
\usage{
PanelMatch(
  panel.data,
  lag,
  refinement.method,
  qoi,
  size.match = 10,
  match.missing = TRUE,
  covs.formula = NULL,
  lead = 0,
  verbose = FALSE,
  exact.match.variables = NULL,
  forbid.treatment.reversal = FALSE,
  matching = TRUE,
  listwise.delete = FALSE,
  use.diagonal.variance.matrix = FALSE,
  restrict.control.period = NULL,
  placebo.test = FALSE
)
}
\arguments{
\item{panel.data}{A \code{PanelData} object containing time series cross sectional data. 
Time data must be sequential integers that increase by 1. Unit identifiers must be integers. Treatment data must be binary.}

\item{lag}{An integer value indicating the length of treatment history periods to be matched on}

\item{refinement.method}{A character string specifying the matching or weighting method to be used for refining the matched sets. The user can choose "mahalanobis", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight", "ps.msm.weight", "CBPS.msm.weight", or "none". The first three methods will use the \code{size.match} argument to create sets of at most \code{size.match} closest control units. Choosing "none" will assign equal weights to all control units in each matched set. The MSM methods refer to marginal structural models. See Imai, Kim, and Wang (2023) for a more in-depth discussion of MSMs.}

\item{qoi}{quantity of interest, provided as a string: \code{att} (average treatment effect on treated units), \code{atc} (average treatment effect of treatment on the control units) \code{art} (average effect of treatment reversal for units that experience treatment reversal), or \code{ate} (average treatment effect).}

\item{size.match}{An integer dictating the number of permitted closest control units in a matched set after refinement. 
This argument only affects results when using a matching method ("mahalanobis" or any of the refinement methods that end in ".match").
This argument is not needed and will have no impact if included when a weighting method is specified (any \code{refinement.method} that includes "weight" in the name).}

\item{match.missing}{Logical variable indicating whether or not units should be matched on the patterns of missingness in their treatment histories. Default is TRUE. When FALSE, neither treated nor control units are allowed to have missing treatment data in the lag window.}

\item{covs.formula}{One sided formula object indicating which variables should be used for matching and refinement. 
Argument is not needed if \code{refinement.method} is set to "none"
If the user wants to include lagged variables, this can be done using a function, "lag()", which takes two, unnamed, 
positional arguments. The first is the name of the variable which you wish to lag. The second is the lag window, 
specified as an integer sequence in increasing order.
For instance, I(lag(x, 1:4)) will then add new columns to the data for variable "x" for time t-1, t-2, t-3, and t-4 internally
and use them for defining/measuring similarity between units. 
Other transformations using the I() function, such as I(x^2) are also permitted.
The variables specified in this formula are used to define the similarity/distances between units.}

\item{lead}{integer sequence specifying the lead window, for which qoi point estimates (and standard errors) will 
ultimately be produced. Default is 0 (which corresponds to contemporaneous treatment effect).}

\item{verbose}{option to include more information about the \code{matched.set} object calculations, 
like the distances used to create the refined sets and weights.}

\item{exact.match.variables}{character vector giving the names of variables to be exactly matched on. These should be time invariant variables. 
Exact matching for time varying covariates is not currently supported.}

\item{forbid.treatment.reversal}{Logical. For the ATT, it indicates whether or not it is permissible for treatment to reverse in the specified lead window. This is defined analogously for the ART. It is not valid for the ATC or ATE. 
When set to TRUE, only matched sets for treated units where treatment is 
applied continuously in the lead window are included in the results. Default is FALSE.}

\item{matching}{logical indicating whether or not any matching on treatment history should be performed. 
This is primarily used for diagnostic purposes, and most users will never need to set this to FALSE. Default is TRUE.}

\item{listwise.delete}{TRUE/FALSE indicating whether or not missing data should be handled using listwise deletion or the package's default missing data handling procedures. Default is FALSE.}

\item{use.diagonal.variance.matrix}{TRUE/FALSE indicating whether or not a regular covariance matrix should be used in mahalanobis distance calculations during refinement, 
or if a diagonal matrix with only covariate variances should be used instead. 
In many cases, setting this to TRUE can lead to better covariate balance, especially when there is 
high correlation between variables. Default is FALSE. This argument is only necessary when 
\code{refinement.method = mahalanobis} and will have no impact otherwise.}

\item{restrict.control.period}{(optional) integer specifying the number of pre-treatment periods that treated units and potentially matched control units should be non-NULL and in the control state. For instance, specifying 4 would mean that the treatment history cannot contain any missing data or treatment from t-4 to t.}

\item{placebo.test}{logical TRUE/FALSE. indicates whether or not you want to be able to run a placebo test. This will add additional requirements on the data -- specifically, it requires that no unit included in the matching/refinement process can having missing outcome data over the lag window. Additionally, you should not use the outcome variable in refinement when \code{placebo.test = TRUE}.}
}
\value{
\code{PanelMatch()} returns an object of class \code{PanelMatch}. This is a list that contains a few specific elements: 
First, a \code{matched.set} object(s) that has the same name as the provided qoi if the qoi is "att", "art", or "atc". 
If qoi = "ate" then two \code{matched.set} objects will be attached, named "att" and "atc." Please consult the documentation for
\code{matched_set()} to read more about the structure and usage of \code{matched.set} objects. 
The \code{PanelMatch} object also has some additional attributes that track metadata about the specification, like the names of the unit and time identifier variables.
}
\description{
\code{PanelMatch} identifies treated observations and a matched set for each treated
observation. Specifically, for a given treated unit, the matched
set consists of control observations that have an identical
treatment history up to a number of \code{lag}
time periods. A further refinement of
the matched set using matching or weighting techniques, described below.
}
\examples{
dem.sub <- dem[dem[, "wbcode2"] <= 100, ]
dem.sub.panel <- PanelData(dem.sub, "wbcode2", "year", "dem", "y")
# create subset of data for simplicity
PM.results <- PanelMatch(panel.data = dem.sub.panel, lag = 4, 
                         refinement.method = "ps.match", 
                         match.missing = TRUE, 
                         covs.formula = ~ tradewb,
                         size.match = 5, qoi = "att",
                         lead = 0:4, 
                         forbid.treatment.reversal = FALSE)
# include lagged variables
PM.results <- PanelMatch(panel.data = dem.sub.panel, lag = 4, 
                         refinement.method = "ps.weight", 
                         match.missing = TRUE, 
                         covs.formula = ~ tradewb + I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                         size.match = 5, qoi = "att",
                         lead = 0:4, 
                         forbid.treatment.reversal = FALSE)

}
\references{
Imai, Kosuke, In Song Kim, and Erik Wang (2023)
}
\author{
Adam Rauh <amrauh@umich.edu>, In Song Kim <insong@mit.edu>, Erik Wang
<haixiao@Princeton.edu>, and Kosuke Imai <imai@harvard.edu>
}
