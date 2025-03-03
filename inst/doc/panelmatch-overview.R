## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(PanelMatch)
dem.panel <- PanelData(panel.data = dem, 
                       unit.id = "wbcode2", 
                       time.id = "year", 
                       treatment = "dem", 
                       outcome = "y")

## -----------------------------------------------------------------------------
dem.panel <- PanelData(panel.data = dem,
               unit.id = "wbcode2",
               time.id = "year",
               treatment = "dem",
               outcome = "y")
DisplayTreatment(panel.data = dem.panel, legend.position = "none",
                 xlab = "year", ylab = "Country Code", 
                 hide.x.tick.label = TRUE, hide.y.tick.label = TRUE)

## -----------------------------------------------------------------------------
PM.maha <- PanelMatch(panel.data = dem.panel, 
                      lag = 4, 
                      refinement.method = "mahalanobis",
                      match.missing = FALSE, 
                      covs.formula = ~ I(lag(tradewb, 0:4)) + 
                                       I(lag(y, 1:4)),
                      size.match = 5, 
                      qoi = "att", 
                      lead = 0:2,
                      use.diagonal.variance.matrix = TRUE,
                      forbid.treatment.reversal = FALSE)



PM.ps.weight <- PanelMatch(lag = 4, 
                      refinement.method = "ps.weight",
                      panel.data = dem.panel, 
                      match.missing = FALSE, 
                      covs.formula = ~ I(lag(tradewb, 0:4)) + 
                                       I(lag(y, 1:4)),
                      qoi = "att", 
                      lead = 0:2,
                      use.diagonal.variance.matrix = TRUE,
                      forbid.treatment.reversal = FALSE)

## -----------------------------------------------------------------------------
plot(PM.maha)

## -----------------------------------------------------------------------------
covbal <- get_covariate_balance(PM.maha, PM.ps.weight, 
                                panel.data = dem.panel,
                                covariates = c("tradewb", "y"),
                                include.unrefined = TRUE)

## -----------------------------------------------------------------------------
summary(covbal)
plot(covbal, type = "panel", 
     include.unrefined.panel = FALSE, ylim = c(-.5, .5))
# Since specifications are identical except 
# for refinement method, just look at the first result.
plot(get_unrefined_balance(covbal)[1], 
     include.unrefined.panel = FALSE, ylim = c(-.5, .5))

## -----------------------------------------------------------------------------
PE.results <- PanelEstimate(sets = PM.maha, 
                            panel.data = dem.panel, 
                            se.method = "bootstrap")

plot(PE.results)

