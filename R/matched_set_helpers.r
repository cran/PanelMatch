#' findBinaryTreated
#'
#' \code{findBinaryTreated} is used to identify t,id pairs of units for which a matched set might exist.
#' More precisely, it finds units for which at time t, the specified treatment has been applied, but at time t - 1, the treatment has not.
#'
#' @param dmat Data frame or matrix containing data used to identify potential treated units. Must be specified in such a way that a combination of time and id variables will correspond to a unique row. Must also contain at least a binary treatment variable column as well.
#' @param treatedvar Character string that identifies the name of the column in \code{dmat} that provides information about the binary treatment variable
#' @param time.var Character string that identifies the name of the column in \code{dmat} that contains data about the time variable. This data must be integer that increases by one.
#' @param unit.var Character string that identifies the name of the column in \code{dmat} that contains data about the variable used as a unit id. This data must be integer
#' @param hasbeensorted variable that only has internal usage for optimization purposes. There should be no need for a user to toggle this
#'
#' @return \code{findBinaryTreated} returns a subset of the data in the \code{dmat} data frame, containing only treated units for which a matched set might exist
#'
#' @keywords internal
#'
findBinaryTreated <- function(dmat, qoi.in,
                              treatedvar, 
                              time.var, 
                              unit.var, 
                              hasbeensorted = FALSE)
{
  dmat <- dmat[, c(unit.var, time.var, treatedvar)]
  #subset the columns to just the data needed for this operation

  colidx <- which(colnames(dmat) == treatedvar)
  if (length(colidx) > 1) stop("error in column naming scheme")
  uidx <- which(colnames(dmat) == unit.var)
  if (length(uidx) > 1) stop("error in column naming scheme")

  if (hasbeensorted)
  {
    odf <- dmat
  }
  else
  {
    odf <- dmat[order(dmat[,unit.var], dmat[,time.var]), ]
  }
  classes <- sapply(dmat, class)
  
  if (classes[time.var] != "integer")
  {
    warning("time variable data provided not integer. Automatic conversion attempted")
    dmat[, time.var] <- as.integer(dmat[, time.var])
  }
  if (classes[unit.var] != "integer")
  {
    stop("unit id variable data provided not integer")
  }
  
  if (identical(qoi.in, "atc")) 
  {
    
    t.history <- odf[, treatedvar]
    id.history <- odf[, unit.var]
    c.idxs <- which(t.history == 0)
    t1 <- c.idxs - 1
    
    t1[t1 == 0] <- NA
    
    idx <- t.history[c.idxs] == 0 & 
      t.history[t1] == 0 & 
      (id.history[c.idxs] == id.history[t1])
    idx[is.na(idx)] <- FALSE
    odf <- odf[c.idxs[idx],]
    rownames(odf) <- NULL
  } else {
    t.history <- odf[,treatedvar]
    t.idxs <- which(t.history == 1)
    
    num.df <- as.matrix(odf)
    if (!is.numeric(num.df)) stop("data in treated, time, or id columns is not numeric")
    
    ind <- get_treated_indices(num.df, t.idxs - 1, colidx - 1, uidx - 1)
    treated.unit.indices <- t.idxs[ind]
    odf <- odf[treated.unit.indices, ]
    rownames(odf) <- NULL
  }
  
  return(odf)
}


#' get.matchedsets
#'
#' \code{get.matchedsets} is used to identify matched sets for a given unit with a specified i, t.
#'
#' @param t integer vector specifying the times of treated units for which matched sets should be found. This vector should be the same length as the following \code{id} parameter -- the entries at corresponding indices in each vector should form the t,id pair of a specified treatment unit.
#' @param id integer vector specifying the unit ids of treated units for which matched sets should be found. note that both \code{t} and \code{id} can be of length 1
#' @param L An integer value indicating the length of treatment history to be matched
#' @param data data frame containing the data to be used for finding matched sets.
#' @param t.column Character string that identifies the name of the column in \code{data} that contains data about the time variable. Each specified entry in \code{t} should be somewhere in this column in the data. This data must be integer that increases by one.
#' @param id.column Character string that identifies the name of the column in \code{data} that contains data about the unit id variable. Each specified entry in \code{id} should be somewhere in this column in the data. This data must be integer.
#' @param treatedvar Character string that identifies the name of the column in \code{data} that contains data about the binary treatment variable.
#' @param hasbeensorted variable that only has internal usage for optimization purposes. There should be no need for a user to toggle this
#' @param match.on.missingness TRUE/FALSE indicating whether or not the user wants to "match on missingness." That is, should units with NAs in their treatment history windows be matched with control units that have NA's in corresponding places?
#' @param matching logical indicating whether or not the treatment history should be used for matching. This should almost always be set to TRUE, except for specific situations where the user is interested in particular diagnostic questions.
#' @return \code{get.matchedsets} returns a "matched set" object, which primarily contains a named list of vectors. Each vector is a "matched set" containing the unit ids included in a matched set. The list names will indicate an i,t pair (formatted as "<i variable>.<t variable>") to which the vector/matched set corresponds.
#'
#'
#' @keywords internal
#'
get.matchedsets <- function(t, id, data, L, t.column, id.column, treatedvar,
                            hasbeensorted = FALSE, match.on.missingness = TRUE,
                            matching = TRUE, 
                            qoi.in,
                            restrict.control.period = NULL)
{

  if (length(t) == 0 || length(id) == 0)
  {
    stop("time and/or unit information missing")
  }
  if (!hasbeensorted)
  {
    data <- data[order(data[,id.column], data[,t.column]), ]

  }
  classes <- sapply(data, class)
  if (classes[t.column] != "integer")
  {
    warning("time variable data provided not integer. Automatic conversion attempted")
    data[, t.column] <- as.integer(data[, t.column])
  }
  if (classes[id.column] != "integer")
  {
    stop("unit id variable data provided not integer")
  }

  d <- data[, c(id.column, t.column, treatedvar)]
  d <- as.matrix(d)
  if (!is.numeric(d)) stop('data in treated, time, or id columns is not numeric')

  compmat <- data.table::dcast(data.table::as.data.table(d), 
                               formula = paste0(id.column, "~", t.column),
                               value.var = treatedvar) 
  #reshape the data so each row corresponds to a unit, columns specify treatment over time
  
  
  if (match.on.missingness)
  {
    d[is.na(d[,treatedvar]), treatedvar] <- -1
    compmat[is.na(compmat)] <- -1
  }
  
  control.histories <- get_comparison_histories(d, t, id, which(colnames(d) == t.column) - 1 ,
                                                which(colnames(d) == id.column) - 1, L,
                                                which(colnames(d) == treatedvar) - 1,
                                                identical(qoi.in, "atc")) 
  #control histories should be a list
  
  if (!is.null(restrict.control.period))
  {
    indx <- enforce_strict_histories(control.histories, 
                                     restrict.control.period)
    control.histories <- control.histories[indx]
    t <- t[indx]
    id <- id[indx]
  }
  
  if (!matching & !match.on.missingness)
  {
    tidx <- !unlist(lapply(control.histories, 
                           function(x)return(any(is.na(x)))))
    control.histories <- control.histories[tidx]
    t2 <- t[!tidx]
    id2 <- id[!tidx]
    t <- t[tidx]
    id <- id[tidx]
    t.map <- match(t, unique(d[, t.column]))
    sets <- non_matching_matcher(control.histories, as.matrix(compmat), t.map, 
                                 id, L = 1, missing_window = L)
    if (any(!tidx))
    {
      l2 <- replicate(sum(!tidx), numeric())
      named.sets <- matched_set(matchedsets = c(sets, l2), 
                                id = c(id, id2), 
                                t = c(t, t2), L = L,
                                t.var = t.column, 
                                id.var = id.column, 
                                treatment.var = treatedvar)
    } else
    {
      named.sets <- matched_set(matchedsets = sets, 
                                id = id, t = t, L = L,
                                t.var = t.column, id.var = id.column, 
                                treatment.var = treatedvar)
    }
  }
  else
  {
    t.map <- match(t, unique(d[, t.column]))
    sets <- get_msets_helper(control.histories, 
                             as.matrix(compmat), 
                             t.map, id, L)
    named.sets <- matched_set(matchedsets = sets, 
                              id = id, t = t, L = L,
                              t.var = t.column, 
                              id.var = id.column, 
                              treatment.var = treatedvar)
  }
  attr(named.sets, "restrict.control.period") <- restrict.control.period
  return(named.sets)
   
}


#' extract_differences
#' This function calculates the differences from t-1 to 1 for treated and control units in the treatment variable. While functionality is somewhat trivial for current implementation of package, it will be needed for continuous treatment version of the package.
#' @param indexed.data data that has been indexed. Rows have been named with a unique identifier.
#' @param matched.set matched.set object
#' @param treatment.variable string specifying treatment variable
#' @param qoi string specifying QOI
#'
#' @return matched.set object, with differences extracted as described previously for each matched set.
#' @keywords internal
extract_differences <- function(indexed.data, matched.set, 
                                treatment.variable,
                                qoi)
{
  treated.t <- as.integer(sub(".*\\.", "", names(matched.set)))
  treated.id <- as.integer(sub("\\..*", "", names(matched.set)))
  treated.tm1 <- treated.t - 1
  
  
  treated.key.t <- names(matched.set)
  treated.key.tm1 <- paste0(treated.id, ".", treated.tm1)
  
  multi.factor <- 1 
  if(length(matched.set[[1]]) > 0)
  {
    control.keys.t <- paste0(matched.set[[1]], ".", treated.t)
    control.keys.tm1 <- paste0(matched.set[[1]], ".", treated.tm1)
    
    keys.t <- c(treated.key.t, control.keys.t)
    keys.tm1 <- c(treated.key.tm1, control.keys.tm1)
    
    differences <- as.numeric(indexed.data[keys.t, treatment.variable] - 
                                indexed.data[keys.tm1, treatment.variable])
    
    attr(matched.set[[1]], "treatment.change") <- differences[1] * multi.factor
     
    attr(matched.set[[1]], "control.change") <- differences[2:length(differences)] * multi.factor
    
  } else {
    differences <- as.numeric(indexed.data[treated.key.t, treatment.variable] - 
                                indexed.data[treated.key.tm1,
                                             treatment.variable])
    attr(matched.set[[1]], "treatment.change") <- differences[1] * multi.factor
  }
  return(matched.set[[1]])
  
}

#' identifyDirectionalChanges
#' Identifies changes in treatment variable for treated and control observations
#' @param msets 
#' @param ordered.data 
#' @param id.var 
#' @param time.var 
#' @param treatment.var 
#' @param qoi 
#'
#' @return matched.set object with changes in the treatment variable for treated and control observations identified.
#' @keywords internal
identifyDirectionalChanges <- function(msets, ordered.data, id.var, time.var,
                                       treatment.var, qoi)
{
  rownames(ordered.data) <- paste0(ordered.data[, id.var], 
                                   ".", 
                                   ordered.data[, time.var])
  
  for (i in 1:length(msets)) {
      
    msets[[i]] <- extract_differences(ordered.data, msets[i], 
                                      treatment.var, qoi)
      
  }
  return(msets)
  
}

#' expand_treated_ts
#' Builds a list that contains all times in a lag window that correspond to a particular treated unit. This is structured as a list of vectors. Each vector is lag + 1 units long. The overall list will be the same length as the number of matched sets
#' @param lag lag value
#' @param treated.ts times of treated observations
#' @return list. Contains all times in a lag window that correspond to a particular treated unit
#' @keywords internal
expand_treated_ts <- function(lag, treated.ts)
{

  helper <- function(treated.t)
  {
    return(seq(from =  (treated.t - lag), 
               to = treated.t, by = 1))
  }
  lapply(treated.ts, helper)
}