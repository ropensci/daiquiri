# Functions for plotting aggregated data
#

# TODO: set global variables for consistent colour scheme, e.g. red4 for norecords


# -----------------------------------------------------------------------------
#' Plot data for all aggregated fields
#'
#' Create individual plots for each data field showing any changepoints identified
#'
#' @param cpdaggregate A \code{cpdaggregate} object
#' @param save_plot Save plot to disk. Currently a bit pointless. What other options would there be?
#' @param save_directory String. Relative path for save folder, use double blackslashes for nested folders and end with double backslash
#' @param save_filetype String. Filetype extension supported by \code{ggplot2}, e.g. tif, pdf
#' @param showprogress Print progress to console. Default = FALSE
#' @return A \code{cpdaggregate} object
#' @export
plot_aggregated_data <- function(cpdaggregate, save_plot = TRUE, save_directory = NULL, save_filetype = "png", showprogress = FALSE){
	# TODO: decide whether to set a class on the aggregated data and use the generic plot method instead - need to distinguish bettween wanting overall summary or individual plots
	# TODO: put some of the repeated ggplot2::ggplot calls into functions
	# TODO: provide option to plot changepoints or not?
	# TODO: currently, save_plot = FALSE is a bit pointless. Do we want to allow this to be used interactively and open plots in device?
	#temp assignment
  # cpdaggregate<-testcpddata_byday
  # save_plot = TRUE
  # save_directory = ".\\testoutput\\"
  # save_filetype = "png"
  # showprogress = TRUE

  aggfields <- cpdaggregate$aggregatefields

  if(showprogress) cat("Plotting changepoint summary...","\n")
  plot_changepoint_summary(cpdaggregate, changepoint_methods = "all", save_plot = save_plot, save_directory = save_directory, save_filetype = save_filetype, showprogress = showprogress)

  for(i in 1:length(aggfields)){
    #    i = 5
    if(showprogress) cat(i, ":", names(aggfields[i]),"\n")
    # use timepoint column to reflect overall counts
    if( is.fieldtype_timepoint(aggfields[[i]]$fieldtype)){
      if(showprogress) cat("Plotting overall counts...","\n")
      timepointcolname <- names(aggfields[[i]]$values[1])
      aggtype <- "n"
      g <- ggplot2::ggplot(aggfields[[i]]$values[c(timepointcolname, aggtype)], ggplot2::aes_string(timepointcolname, aggtype)) +
      	ggplot2::scale_x_date(date_breaks = "1 month") +
      	ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      	ggplot2::xlab(timepointcolname) +
      	ggplot2::ylab("Number of records") +
      	ggplot2::ggtitle("Overall records") +
      	ggplot2::geom_col()
      # specify y axis scale
      aggbreaks <- yscale_breaks(aggtype, max(aggfields[[i]]$values[[aggtype]], na.rm = TRUE), min(aggfields[[i]]$values[[aggtype]], na.rm = TRUE), aggfields[[i]]$fieldtype)
      if( !is.na(aggbreaks[1]) ){
        g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks, limits = c(aggbreaks[1], max(max(aggfields[[i]]$values[[aggtype]], na.rm = TRUE), aggbreaks[length(aggbreaks)])))
      }

      # add changepoint lines if available
      cpts <- aggfields[[i]]$changepoints[[aggtype]]
      # TODO: add multiple sets of lines if more than one method used
      for(k in 1:length(cpts)){
        # TODO: different colour per method
        g <- g + ggplot2::geom_vline(xintercept=cpts[[k]]$changepoint_timepoints, colour = changepoint_colour(names(cpts[k])))
      }

      if(save_plot){
        ggplot2::ggsave(paste0(save_directory, "Overall_n.", save_filetype))
      }
    } else{
      # plot counts for this field against overall counts in that timepoint
      if(showprogress) cat("Plotting values present out of total records...","\n")
      timepointcolname <- names(aggfields[[i]]$values[1])
      totalrecords <- rowSums(aggfields[[i]]$values[c("n","missing_n")], na.rm = TRUE)
      aggbreaks <- yscale_breaks("n", max(totalrecords, na.rm = TRUE), min(totalrecords, na.rm = TRUE))
      # TODO: consider reshaping dataset to long so can use stack option (which might make the legending easier)
      g <- ggplot2::ggplot(data.frame(aggfields[[i]]$values[c(timepointcolname,"n","missing_n")], totalrecords)) +
      	ggplot2::geom_col(ggplot2::aes_string(x = timepointcolname, y = "totalrecords"), fill = "red4") +
      	ggplot2::geom_col(ggplot2::aes_string(x = timepointcolname, y = "n")) +
      	ggplot2::scale_x_date(date_breaks = "1 month") +
      	ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      	ggplot2::xlab(names(aggfields[[i]]$values[1])) +
      	ggplot2::ylab("Number of records") +
      	ggplot2::ggtitle(names(aggfields[i])) +
      	ggplot2::scale_y_continuous(breaks = aggbreaks, limits = c(aggbreaks[1], max(max(totalrecords, na.rm = TRUE), aggbreaks[length(aggbreaks)])))

      if(save_plot){
        ggplot2::ggsave(paste0(save_directory, names(aggfields[i]), "_", "overall",  ".", save_filetype))
      }

      # individual plots per aggtype
      if(showprogress) cat("Plotting individual aggregatetypes:","\n")
      for(j in 2:length(aggfields[[i]]$values)){
        #j=5
        if(showprogress) cat("  ", names(aggfields[[i]]$values[j]),"\n")
        plot_aggregatefield(aggfields[[i]], names(aggfields[[i]]$values[j]), changepoint_methods = "all", save_plot = save_plot, save_directory = save_directory, save_filetype = save_filetype)
      }
    }
  }
}

# -----------------------------------------------------------------------------
# create an individual plot according to aggregatetype
# optionally plot changepoints - "all"/"none"/vectorofmethodnames
plot_aggregatefield <- function(aggfield, aggtype, changepoint_methods = "all", save_plot = TRUE, save_directory = NULL, save_filetype = "png"){
  #temp assignment
  #  aggfield<-testcpddata_byday$aggregatefields[[1]]
  # aggtype = "n"
  # save_plot = TRUE
  # save_directory = ".\\testoutput\\"
  # save_filetype = "png"
  #changepoint_methods = "all"

  # TODO: validate/reformat save parameters

  timepointcolname <- names(aggfield$values[1])
  # set up universal plot characteristics
  g <- ggplot2::ggplot(aggfield$values[c(timepointcolname, aggtype)], ggplot2::aes_string(timepointcolname, aggtype)) +
  	ggplot2::scale_x_date(date_breaks = "1 month") +
  	ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  	ggplot2::xlab(timepointcolname) +
  	ggplot2::ylab(aggtype) +
  	ggplot2::ggtitle(aggfield$columnname)

  # specify type of plot
  if( aggtype %in% c("n","missing_n","missing_perc","distinct") ){
    g <- g + ggplot2::geom_col()
  } else if( aggtype %in% c("min","max") ){
    g <- g + ggplot2::geom_point()
  } else {
    stop(paste("Unknown aggregate type:", aggtype))
  }

  # specify y axis scale
  aggbreaks <- yscale_breaks(aggtype, max(aggfield$values[[aggtype]], na.rm = TRUE), min(aggfield$values[[aggtype]], na.rm = TRUE), aggfield$fieldtype)
  if( !is.na(aggbreaks[1]) ){
    g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks, limits = c(aggbreaks[1], max(max(aggfield$values[[aggtype]], na.rm = TRUE), aggbreaks[length(aggbreaks)])))
  }

  # add changepoint lines if requested
  if( changepoint_methods != "none"){
    cpts <- aggfield$changepoints[[aggtype]]
    for(k in 1:length(cpts)){
      if( names(cpts[k]) %in% changepoint_methods || changepoint_methods == "all" ){
        g <- g + ggplot2::geom_vline(xintercept=cpts[[k]]$changepoint_timepoints, colour = changepoint_colour(names(cpts[k])))
      }
    }
  }

  if(save_plot){
    ggplot2::ggsave(paste0(save_directory, aggfield$columnname, "_", aggtype,  ".", save_filetype))
  }

}

# -----------------------------------------------------------------------------
#' Plot overall changepoint summary
#'
#' Create a single plot showing changepoints identified in all data fields of aggregated object
#'
#' @param cpdaggregate A \code{cpdaggregate} object
#' @param changepoint_methods Vector of shortnames of changepoint methods to include or exclude from plot
#' @param save_plot Save plot to disk. If FALSE, opens plot in device instead
#' @param save_directory String. Relative path for save folder, use double blackslashes for nested folders and end with double backslash
#' @param save_filetype String. Filetype extension supported by \code{ggplot2}, e.g. tif, pdf
#' @param save_filename String. Filename for plot, excluding extension
#' @param showprogress Print progress to console. Default = FALSE
#' @return A \code{cpdaggregate} object
#' @export
plot_changepoint_summary <- function(cpdaggregate, changepoint_methods = "all", save_plot = TRUE, save_directory = NULL, save_filetype = "png", save_filename = "Changepoint_summary", showprogress = FALSE){
	# TODO: Do we want to differentiate between changepoints that are only relevant to specific field and those that are due to global reasons e.g. totally missing records?
	# TODO: inconsistent implementation of changepoint_methods param compared to plot_aggregatefield
	#temp assignment
  #  cpdaggregate <- testcpddata_byday
  # save_plot = TRUE
  # save_directory = ".\\testoutput\\"
  # save_filetype = "png"
  # save_filename = NULL
  # changepoint_methods = "all"
  # showprogress = TRUE
  #changepoint_methods = c("cpt.var")
  #changepoint_methods = c("-is_na", "-is_zero")
  #changepoint_methods = c("-is_na")

  # TODO: validate/reformat save parameters

  aggfields<-cpdaggregate$aggregatefields

  # filter changepoints by chosen methods
  # can't have a mixture of positives and negatives
  # TODO: see how subsetting works in base
  # TODO: move to separate function
  if(changepoint_methods[1] == "all"){
    cptdf <- cpdaggregate$changepoints_df
  } else{
    cpmethodsall <- unique(cpdaggregate$changepoints_df$changepoint_method)
    if( substring(changepoint_methods[1], 1, 1) == "-" ){
      for(c in 1:length(changepoint_methods)){
        cpmethodsall <- grep(substring(changepoint_methods[c], 2), cpmethodsall, ignore.case = TRUE, value = TRUE, invert = TRUE)
      }
      cpmethods <- cpmethodsall
    } else {
      cpmethods <- character()
      for(c in 1:length(changepoint_methods)){
        cpmethods <- c(cpmethods, grep(changepoint_methods[c], cpmethodsall, ignore.case = TRUE, value = TRUE))
      }
    }
    cptdf <- cpdaggregate$changepoints_df[cpdaggregate$changepoints_df$changepoint_method %in% cpmethods,]
  }

  # aggregate by aggfield
  # TODO: consider scaling by total number of changepoint methods/aggtypes
  # TODO: consider adding an overall category which sums across all the fields
  cptedges <- cptdf[cptdf$changepointtype == "edge", c("fieldname","timepoint")]
  cptedgesagg <- stats::aggregate(cptedges, by = list(fieldname=cptedges$fieldname, timepoint=cptedges$timepoint), FUN = length)[,1:3]
  names(cptedgesagg)[3] <- "numchangepoints"

  # TODO: set individual min/max dates per aggfield? Probably shouldn't have any edges in sections where there are no records for that aggfield
  cptbase <- data.frame(fieldname=names(aggfields),
                          timepointmin=min(aggfields[[1]]$values[[1]]),
                          timepointmax=max(aggfields[[1]]$values[[1]]),
                          stringsAsFactors = FALSE)
  timepointcolname <- names(aggfields[[1]]$values[1])

  # specify full span of timepoint field as x-axis
  g <- ggplot2::ggplot(cptbase) +
  	ggplot2::geom_segment(ggplot2::aes_string(x="timepointmin", xend="timepointmax", y="fieldname", yend="fieldname"),
                 linetype="solid",
                 size=0.1,
                 col="grey70") +
  	ggplot2::scale_x_date(date_breaks = "1 month") +
  	ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
#    theme(aspect.ratio = 0.5) +
  	ggplot2::theme(legend.position = "none") +
  	ggplot2::xlab(timepointcolname) +
  	ggplot2::ylab("") +
  	ggplot2::ggtitle("Changepoint summary")

  # block out sections with no records
  # TODO: not sure if should allow non-plotting of zeros
  cptzeros <- cptdf[cptdf$aggregatetype == "n" & cptdf$changepoint_method == "is_zero {internal}" & cptdf$changepointtype == "istrue", c("fieldname","timepoint")]
  # per field
  g <- g + ggplot2::geom_point(data=cptzeros, ggplot2::aes_string(x="timepoint", y="fieldname"), size=0.1, col = "red4")
  # overall
  g <- g + ggplot2::geom_vline(xintercept=cptzeros[cptzeros$fieldname == cpdaggregate$timepoint_fieldname, "timepoint"],
                      colour = "red3")

  # add changepoints
  g <- g + ggplot2::geom_point(data=cptedgesagg, ggplot2::aes_string(x="timepoint", y="fieldname", size="numchangepoints"), col="steelblue")

  # TODO: add list of changepoint methods outside of plot area
  # bit tricky, maybe leave to markdown
  # xpos <- max(aggfields[[1]]$values[[1]]) + 5
  # g <- g + theme(plot.margin = unit(c(1,5,1,1), "lines")) +
  #   annotation_custom(grob = textGrob(label = cpmethods, hjust = 0),
  #                     xmin = xpos, xmax = xpos
  #                     )


  if(save_plot){
    ggplot2::ggsave(paste0(save_directory, save_filename, ".", save_filetype))
  } else{
  	g
  }

}

yscale_breaks <- function(aggtype, maxval, minval = 0, fieldtype = NULL){
  breaks <- NA

  if( aggtype %in% c("distinct","n","missing_n") ){
    rangesize <- floor(log10(maxval))
    if( rangesize == 0 || maxval == 0){
      breaks <- seq(0, max(maxval, 5))
    }
    else{
      #      ymax <- ceiling(maxval/10^rangesize)*10^rangesize
      ymax <- ceiling(maxval/10^(rangesize-1))*10^(rangesize-1)
      breaks <- seq(0, ymax, by = 10^rangesize)
    }
  } else if( aggtype %in% c("missing_perc") ){
    breaks <- seq(0, 100, by = 10)
  } else if( aggtype %in% c("min","max") ){
    if( is.fieldtype_number(fieldtype) ){
      if( maxval < minval ){
        # TODO: maybe better to return a warning and/or a graph with the error written on it rather than stopping altogether
        stop(paste0("Invalid parameter(s) supplied: minval [", toString(minval),"] is greater than maxval [", toString(maxval),"]"), call. = FALSE)
      }
      rangesize <- floor(log10(maxval - minval))
      if( rangesize == 0 || (maxval == 0 && minval == 0) ) {
        breaks <- seq(floor(minval), max(ceiling(maxval), 5))
      }
      else{
        #        ymin <- floor(minval/10^rangesize)*10^rangesize
        #        ymax <- ceiling(maxval/10^rangesize)*10^rangesize
        ymin <- floor(minval/10^(rangesize-1))*10^(rangesize-1)
        ymax <- ceiling(maxval/10^(rangesize-1))*10^(rangesize-1)
        breaks <- seq(ymin, ymax, by = 10^rangesize)
      }
    } else if( is.fieldtype_datetime(fieldtype) ){

    }
  }
  breaks
}

# TODO: deal with instances of same method with different parameters?
changepoint_colour <- function(changepoint_method){
  switch(changepoint_method,
         "cptvar" = "orange",
         "is_zero" = "blue",
         "is_na" = "green"
  )
}

