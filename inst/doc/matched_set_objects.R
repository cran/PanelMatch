## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(PanelMatch)
uid <-unique(dem$wbcode2)[1:10]
subdem <- dem[dem$wbcode2 %in% uid, ]
DisplayTreatment(unit.id = "wbcode2", time.id = "year", treatment = 'dem', data = subdem)

## -----------------------------------------------------------------------------

PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2", 
                         treatment = "dem", refinement.method = "none", 
                         data = subdem, match.missing = TRUE, 
                         qoi = "att" ,outcome.var = "y",
                         lead = 0, forbid.treatment.reversal = FALSE)

#Extract the matched.set object 
msets <- PM.results$att                         


## -----------------------------------------------------------------------------
# observe the naming scheme
names(msets)

#data frame printing view: useful as a summary view with large data sets
# first column is unit id variable, second is time variable, and 
# third is the number of controls in that matched set
print(msets)

# prints as a list, shows all data at once
print(msets, verbose = TRUE)

## -----------------------------------------------------------------------------
# weights for control units in first matched set
attr(msets[[1]], "weights")

## -----------------------------------------------------------------------------
msets[1]

#prints the control units in this matched set
msets[[1]]


msets["4.1992"] #equivalent to msets[1]

msets[["4.1992"]] #equivalent to msets[[1]]


## -----------------------------------------------------------------------------
plot(msets, xlim = c(0, 4))

# Use full data
PM.results.full <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2", 
                         treatment = "dem", refinement.method = "none", 
                         data = dem, match.missing = TRUE, 
                         qoi = "att" ,outcome.var = "y",
                         lead = 0, forbid.treatment.reversal = FALSE)

#Extract the matched.set object 
plot(PM.results.full$att)

## -----------------------------------------------------------------------------
print(summary(msets))

print(summary(msets, verbose = FALSE))

## -----------------------------------------------------------------------------
#pass matched.set object using the `[` operator
DisplayTreatment(unit.id = "wbcode2", time.id = "year", treatment = 'dem', data = subdem, matched.set = msets[1])

# only show matched set units
DisplayTreatment(unit.id = "wbcode2", time.id = "year", treatment = 'dem', 
					data = subdem, matched.set = msets[1], 
					show.set.only = TRUE, y.size = 15, x.size = 13)

