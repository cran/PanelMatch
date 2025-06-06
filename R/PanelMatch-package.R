#' Matching Methods for Causal Inference with Time-Series Cross-Sectional Data
#'
#' Implements a set of methodological tools
#' that enable researchers to apply matching methods to
#' time-series cross-sectional data. Imai, Kim, and Wang
#' (2023) proposes a nonparametric generalization of the
#' difference-in-differences estimator, which does not rely
#' on the linearity assumption as often done in
#' practice. Researchers first select a method of matching
#' each treated observation for a given unit in a
#' particular time period with control observations from
#' other units in the same time period that have a similar
#' treatment and covariate history. These methods include
#' standard matching methods based on propensity score and
#' Mahalanobis distance, as well as weighting methods. Once 
#' matching is done, both short-term and long-term average 
#' treatment effects for the treated observations can be estimated with 
#' standard errors. The package also offers a variety of diagnostic
#' and visualization functions to assess the credibility of results.
#'
#'
#' @name PanelMatch-package
#' @useDynLib PanelMatch
#' @aliases PanelMatch-package 
#' @docType package
#' @author In Song Kim <insong@mit.edu>, Erik Wang
#' <haixiao@Princeton.edu>, Adam Rauh <amrauh@umich.edu>, and Kosuke Imai <imai@harvard.edu>
#' 
#' Maintainer: In Song Kim \email{insong@mit.edu}
#' @references Imai, Kosuke, In Song Kim and Erik Wang. (2023)
#' @keywords internal
"_PACKAGE"
#' @import ggplot2 stats MASS data.table foreach
#' @importFrom Rcpp sourceCpp
#' @importFrom CBPS CBPS
#' @importFrom utils capture.output
#' @importFrom Matrix cBind rBind tcrossprod crossprod Diagonal Matrix drop0
#' @importFrom methods as
#' @importFrom utils flush.console tail object.size
NULL
