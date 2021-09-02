## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(PanelMatch)
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem)

## -----------------------------------------------------------------------------
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem,
                 hide.x.axis.label = TRUE, hide.y.axis.label = TRUE) # axis label options

## -----------------------------------------------------------------------------
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem,
                 hide.x.axis.label = TRUE, hide.y.axis.label = TRUE,
                 dense.plot = TRUE) # setting dense.plot to TRUE

## -----------------------------------------------------------------------------
# Create the matched sets
PM.results.none <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "none",
                         data = dem, match.missing = TRUE,
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)
# Extract the first matched set
mset <- PM.results.none$att[1]

# Use the DisplayTreatment function to visualize the
# treated unit and matched controls.
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem,
                 matched.set = mset, # this way we highlight the particular set
                 show.set.only = TRUE)

## -----------------------------------------------------------------------------
#Call PanelMatch without any refinement
PM.results.none <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "none",
                         data = dem, match.missing = TRUE,
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)
#Extract the matched.set object
msets.none <- PM.results.none$att

#Call PanelMatch with refinement
PM.results.maha <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "mahalanobis",
                         # use Mahalanobis distance
                         data = dem, match.missing = TRUE,
                         covs.formula = ~ tradewb,
                         size.match = 5, qoi = "att" , outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)

msets.maha <- PM.results.maha$att

## -----------------------------------------------------------------------------
print(msets.none)
print(msets.maha)

summary(msets.none)
summary(msets.maha)

## -----------------------------------------------------------------------------
plot(msets.none)

## -----------------------------------------------------------------------------
msets.maha[1] #prints like a matched.set object
msets.maha[[1]] #prints the matched control unit ids.
msets.maha[["4.1992"]] # same behavior as above

## -----------------------------------------------------------------------------
# Examine the weights assinged to control units in first matched set
attr(msets.none[[1]], "weights")
attr(msets.maha[[1]], "weights")

## -----------------------------------------------------------------------------
PM.results.none <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "none",
                         data = dem, match.missing = TRUE,
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)


PM.results.maha <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "mahalanobis",
                         data = dem, match.missing = TRUE,
                         covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)

# listwise deletion used for missing data
PM.results.listwise <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "mahalanobis",
                         data = dem, match.missing = FALSE, listwise.delete = TRUE,
                         covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)

# propensity score based weighting method
PM.results.ps.weight <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "ps.weight",
                         data = dem, match.missing = FALSE, listwise.delete = TRUE,
                         covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

get_covariate_balance(PM.results.none$att,
                      data = dem,
                      covariates = c("tradewb", "y"),
                      plot = FALSE)

get_covariate_balance(PM.results.maha$att,
                      data = dem,
                      covariates = c("tradewb", "y"),
                      plot = FALSE)

get_covariate_balance(PM.results.listwise$att,
                      data = dem,
                      covariates = c("tradewb", "y"),
                      plot = FALSE)

get_covariate_balance(PM.results.ps.weight$att,
                      data = dem,
                      covariates = c("tradewb", "y"),
                      plot = FALSE)



## -----------------------------------------------------------------------------
# Use equal weights
get_covariate_balance(PM.results.ps.weight$att,
                      data = dem,
                      use.equal.weights = TRUE,
                      covariates = c("tradewb", "y"),
                      plot = TRUE,
                      # visualize by setting plot to TRUE
                      ylim = c(-1, 1))

# Compare covariate balance to refined sets
# See large improvement in balance
get_covariate_balance(PM.results.ps.weight$att,
                      data = dem,
                      covariates = c("tradewb", "y"),
                      plot = TRUE,
                      # visualize by setting plot to TRUE
                      ylim = c(-1, 1))

## -----------------------------------------------------------------------------
balance_scatter(non_refined_set = PM.results.none$att,
               refined_list = list(PM.results.maha$att, PM.results.ps.weight$att),
               data = dem,
               covariates = c("y", "tradewb"))

## -----------------------------------------------------------------------------
PE.results <- PanelEstimate(sets = PM.results.ps.weight, data = dem, 
                            se.method = "bootstrap", 
                            number.iterations = 1000,
                            confidence.level = .95)

# View the point estimates
PE.results[["estimates"]]
# View standard errors
PE.results[["standard.error"]]

# use conditional method
PE.results <- PanelEstimate(sets = PM.results.ps.weight, data = dem, 
                            se.method = "conditional", 
                            confidence.level = .95)

# View the point estimates
PE.results[["estimates"]]
# View standard errors
PE.results[["standard.error"]]


## -----------------------------------------------------------------------------
summary(PE.results)

plot(PE.results)

## -----------------------------------------------------------------------------
# add simple moderating variable
dem$moderator <- 0
dem$moderator <- ifelse(dem$wbcode2 > 100, 1, 2)


PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                         treatment = "dem", refinement.method = "mahalanobis",
                         data = dem, match.missing = TRUE,
                         covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)), # lags
                         size.match = 5, qoi = "att", outcome.var = "y",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)

PE.results <- PanelEstimate(sets = PM.results, data = dem, moderator = "moderator")

# Names correspond to
# moderator values
names(PE.results)

# Extract each result
plot(PE.results[[1]])
plot(PE.results[[2]])

