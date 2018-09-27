# Code for creation of changepoints object
# these contain both the (vector) data for the changepoints and the relevant metadata
# they should be added as children of aggregatefield objects
#

# -----------------------------------------------------------------------------
# all changepointresults for an individual aggregatefield
find_changepoints <- function(aggfieldvalues, method = "all"){
  #temp assignment
  #   aggfieldvalues<-testcpddata_byday$aggregatefields[[2]]$values
  # method = "all"

  aggtypes = names(aggfieldvalues[-1])

  # # flat list
  # # initialise list to bigger than needed
  # changepoints <- vector("list", length(aggtypes)*4)
  # index <- 1
  # for(i in 1:length(aggtypes) ){
  #   #    i = 1
  #   # zeros are not meaningful for dates
  #   if( aggtypes[i] %in% c("n","missing_n","missing_perc","distinct","min","max") &&
  #       (!(inherits(aggfieldvalues[[aggtypes[i]]], "POSIXct") || inherits(aggfieldvalues[[aggtypes[i]]], "Date"))) ){
  #     changepoints[[index]] <- changepoints_is_zero(aggfieldvalues, aggtype = aggtypes[i], nainterpretation = "ignore")
  #     names(changepoints)[index] <- paste0(aggtypes[i], "__is_zero")
  #     index <- index + 1
  #     }
  #   if( aggtypes[i] != "n" ){
  #     changepoints[[index]] <- changepoints_is_na(aggfieldvalues, aggtype = aggtypes[i])
  #     names(changepoints)[index] <- paste0(aggtypes[i], "__is_na")
  #     index <- index + 1
  #   }
  #   if( 1==1 ){
  #     changepoints[[index]] <- changepoints_cptvar(aggfieldvalues, aggtype = aggtypes[i])
  #     names(changepoints)[index] <- paste0(aggtypes[i], "__cptvar")
  #     index <- index + 1
  #   }
  # }
  # # truncate to last non-NULL element
  # changepoints <- changepoints[1:index-1]

  changepoints <- vector("list", length(aggtypes))
  for(i in 1:length(aggtypes) ){
    #    i = 1
    if( aggtypes[i] == "n"){
      # TODO: timepoint field should only do is_zero
      changepoints[[i]] <- list(cptvar=changepoints_cptvar(aggfieldvalues, aggtype = aggtypes[i]),
                                is_zero=changepoints_is_zero(aggfieldvalues, aggtype = aggtypes[i])
      )
    } else if( aggtypes[i] %in% c("missing_n","missing_perc","distinct") ){
      changepoints[[i]] <- list(cptvar=changepoints_cptvar(aggfieldvalues, aggtype = aggtypes[i]),
                                is_zero=changepoints_is_zero(aggfieldvalues, aggtype = aggtypes[i], nainterpretation = "ignore"),
                                is_na=changepoints_is_na(aggfieldvalues, aggtype = aggtypes[i])
      )
    } else if( aggtypes[i] %in% c("min","max") ){
      if( inherits(aggfieldvalues[[aggtypes[i]]], "POSIXct") | inherits(aggfieldvalues[[aggtypes[i]]], "Date") ) {
        # zeros are not meaningful for dates
        changepoints[[i]] <- list(cptvar=changepoints_cptvar(aggfieldvalues, aggtype = aggtypes[i]),
                                  is_na=changepoints_is_na(aggfieldvalues, aggtype = aggtypes[i])
        )
      } else{
        changepoints[[i]] <- list(cptvar=changepoints_cptvar(aggfieldvalues, aggtype = aggtypes[i]),
                                  is_zero=changepoints_is_zero(aggfieldvalues, aggtype = aggtypes[i], nainterpretation = "ignore"),
                                  is_na=changepoints_is_na(aggfieldvalues, aggtype = aggtypes[i])
        )
      }
    }
  }
  names(changepoints) <- aggtypes

  structure(
    list(
      changepoints = changepoints
    ),
    class = "changepoints"
  )
}

is.changepoints <- function(x) inherits(x, "changepoints")

# return all changepoints in single dataframe
# TODO: could probably do with a better name
all_changepoints <- function(aggfields, showprogress = FALSE){
  #temp assignment
  #  aggfields<-testcpddata_byday$aggregatefields
  #showprogress = TRUE

  # initialise dataframe
  cptall <- data.frame(timepoint=as.Date(character()),
                       changepointtype=character(),
                       fieldname=character(),
                       aggregatetype=character(),
                       changepoint_method=character(),
                       stringsAsFactors = FALSE)

  # add changepoints to dataframe
  for(i in 1:length(aggfields)){
    #    i = 5
    if(showprogress) cat(i, ":", names(aggfields[i]),"\n")
    # use timepoint column to reflect overall counts
    if( is.fieldtype_timepoint(aggfields[[i]]$fieldtype)){
      #i=2
      # TODO: add a row to dataframe called "Overall"?
    }
    # populate data frame
    cpts <- aggfields[[i]]$changepoints
    for(j in 1:length(cpts)){
      if(showprogress) cat(" ",names(cpts[j]),": ")
      for(k in 1:length(cpts[[j]])){
        if(showprogress) cat(names(cpts[[j]][k]),", ")
        if( cpts[[j]][[k]]$n_changepoints > 0 ){
          cptall <- rbind(cptall,
                            data.frame(timepoint=cpts[[j]][[k]]$changepoint_timepoints,
                                       changepointtype="edge",
                                       fieldname=names(aggfields[i]),
                                       aggregatetype=cpts[[j]][[k]]$aggregatetype,
                                       changepoint_method=cpts[[j]][[k]]$changepoint_method,
                                       changepoint_method_params=cpts[[j]][[k]]$changepoint_method_params,
                                       stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)
        }
        if( !is.null(cpts[[j]][[k]]$n_outliers) && cpts[[j]][[k]]$n_outliers > 0 ){
          cptall <- rbind(cptall,
                          data.frame(timepoint=cpts[[j]][[k]]$outlier_timepoints,
                                     changepointtype="outlier",
                                     fieldname=names(aggfields[i]),
                                     aggregatetype=cpts[[j]][[k]]$aggregatetype,
                                     changepoint_method=cpts[[j]][[k]]$changepoint_method,
                                     changepoint_method_params=cpts[[j]][[k]]$changepoint_method_params,
                                     stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)
        }
        if( !is.null(cpts[[j]][[k]]$n_istrues) && cpts[[j]][[k]]$n_istrues > 0 ){
          cptall <- rbind(cptall,
                          data.frame(timepoint=cpts[[j]][[k]]$istrue_timepoints,
                                     changepointtype="istrue",
                                     fieldname=names(aggfields[i]),
                                     aggregatetype=cpts[[j]][[k]]$aggregatetype,
                                     changepoint_method=cpts[[j]][[k]]$changepoint_method,
                                     changepoint_method_params=cpts[[j]][[k]]$changepoint_method_params,
                                     stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)
        }
      }
      if(showprogress) cat("\n")
    }
  }
  cptall
}

# -----------------------------------------------------------------------------
# individual functions depending on changepoint method, all return same class

# TODO: maybe should call this cptvar_plus since we're not just running it on the raw data
# TODO: consider adding a scaling factor as cptvar doesn't seem to work well on small numbers
#       (e.g. when converted values from proportions to percentages, it found changepoints only in the latter)
changepoints_cptvar <- function(aggfieldvalues, aggtype){
  #temp assignment
  #  aggfieldvalues<-testcpddata_byday$aggregatefields[[1]]$values
  # aggtype = "n"
  # aggtype = "missing_n"
  #aggtype = "min"

  valuevector <- aggfieldvalues[[aggtype]]

  # replace NAs with a value well outside the range
  if( is.numeric(valuevector)){
    valuevector[is.na(valuevector)] <- (abs(max(valuevector, na.rm = TRUE))+100)*2
  } else{
      if( inherits(valuevector, "POSIXct") ) {
        # TODO: not sure if it is OK to set the origin like this
        valuevector[is.na(valuevector)] <- as.POSIXct(0, tz = "UTC", origin = "1900-01-01")
      } else if( inherits(valuevector, "Date") ) {
        valuevector[is.na(valuevector)] <- as.Date("1900-01-01")
      } else {
        stop("cpt.var method can only be used on numeric data")
      }
    # convert data to numeric explicitly (cpt.var complains if data is double)
    valuevector <- as.numeric(valuevector)
  }


  cpts <- changepoint::cpt.var(valuevector, method = "PELT", class = FALSE)
  changepoint_indexes = cpts[-length(cpts)]
  changepoint_timepoints = aggfieldvalues[[1]][changepoint_indexes]

  structure(
    list(
      changepoint_method = "cpt.var {changepoint}",
      changepoint_method_params = "method = PELT",
      n_changepoints = length(changepoint_indexes),
      changepoint_indexes = changepoint_indexes,
      changepoint_timepoints = changepoint_timepoints,
      aggregatetype = aggtype
    ),
    class = "changepointresult"
  )
}

# simply look for any changes from zero values to nonzero values - makes most sense for count data
# treats NA values either as zero, nonzero, neither zero nor nonzero, or ignores them completely
# returnallzero not meaningful if nainterpretation == "neither", don't know whether to ignore or raise an error
# TODO: use generic na.action somehow?
# TODO: need to do some extensive testing
# TODO: if >2 zeros in a row, return changepoints as edges, if <=2, return them as outliers
changepoints_is_zero <- function(aggfieldvalues, aggtype, nainterpretation = "ignore"){
  #temp assignment
  #   aggfieldvalues<-testcpddata_byday$aggregatefields[2]
   # aggtype = "n"
  #  aggtype = "missing_n"
  # nainterpretation = "ignore"
  #nainterpretation = "zero"

  zerovector <- aggfieldvalues[[aggtype]] == 0

  if( nainterpretation == "zero" ){
    zerovector[is.na(zerovector)] <- TRUE
  } else if( nainterpretation == "nonzero" ){
    zerovector[is.na(zerovector)] <- FALSE
  }

  vectorshiftone <- c(zerovector[1], zerovector[1:length(zerovector)-1])
  diff <- zerovector-vectorshiftone
  if( nainterpretation == "ignore" ){
      diff[is.na(diff)] <- 0
  } else if( nainterpretation == "neither" ){
      diff[is.na(diff)] <- 1
  } else{
    # there should be no NAs in zerovector so don't need to do anything further
  }
  changepoint_indexes = which(diff != 0)
  changepoint_timepoints = aggfieldvalues[[1]][changepoint_indexes]

  istrue_indexes <- which(zerovector)
  istrue_timepoints = aggfieldvalues[[1]][istrue_indexes]

  structure(
    list(
      changepoint_method = "is_zero {internal}",
      changepoint_method_params = paste0("nainterpretation = ", nainterpretation),
      n_changepoints = length(changepoint_indexes),
      changepoint_indexes = changepoint_indexes,
      changepoint_timepoints = changepoint_timepoints,
      n_istrues = length(istrue_indexes),
      istrue_indexes = istrue_indexes,
      istrue_timepoints = istrue_timepoints,
      aggregatetype = aggtype
    ),
    class = "changepointresult"
  )
}

# TODO: not sure if this actually adds anything, consider dropping
changepoints_is_na <- function(aggfieldvalues, aggtype){
  #temp assignment
  #   aggfieldvalues<-testcpddata_byday$aggregatefields[3]
  #  aggtype = "n"
  #returnallna = FALSE

  navector <- is.na(aggfieldvalues[[aggtype]])

  vectorshiftone <- c(navector[1], navector[1:length(navector)-1])
  changepoint_indexes = which(navector-vectorshiftone != 0)
  changepoint_timepoints = aggfieldvalues[[1]][changepoint_indexes]

  istrue_indexes <- which(navector)
  istrue_timepoints = aggfieldvalues[[1]][istrue_indexes]

  structure(
    list(
      changepoint_method = "is_na {internal}",
      changepoint_method_params = "",
      n_changepoints = length(changepoint_indexes),
      changepoint_indexes = changepoint_indexes,
      changepoint_timepoints = changepoint_timepoints,
      n_istrues = length(istrue_indexes),
      istrue_indexes = istrue_indexes,
      istrue_timepoints = istrue_timepoints,
      aggregatetype = aggtype
    ),
    class = "changepointresult"
  )
}

is.changepointresult <- function(x) inherits(x, "changepointresult")

