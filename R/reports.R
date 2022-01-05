# Functions for generating data/plots for reports

# -----------------------------------------------------------------------------
#' Generate report
#'
#' Generate report from previously-created sourcedata and aggregatedata objects
#'
#' @param sourcedata A \code{sourcedata} object returned from \code{\link{load_dataset}} function
#' @param aggregatedata An \code{aggregatedata} object returned from \code{\link{aggregate_data}} function
#' @param save_directory String specifying directory in which to save the report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any file extension.
#' If no filename is supplied, one will be automatically generated with the format ehrchangepoints_report_YYMMDD_HHMMSS.
#' @param format File format of the report. Currently only "html" is supported
#' @param showprogress Print progress to console. Default = TRUE
#' @return A string containing the name and path of the saved report
#' @examples sourcedataobj <- load_dataset(
#'   system.file("extdata", "abx2014.csv", package = "ehrchangepoints"),
#'   fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     SourceSystem = ft_categorical(aggregate_by_each_category=TRUE)),
#'   textfile_contains_columnnames = TRUE,
#'   override_columnnames = FALSE,
#'   na = c("","NULL"),
#'   showprogress = TRUE
#' )
#'
#' aggregatedataobj <- aggregate_data(
#'   sourcedataobj,
#'   aggregation_timeunit = "day",
#'   showprogress = TRUE
#' )
#'
#' generate_report(
#'   sourcedataobj,
#'   aggregatedataobj,
#'   save_directory = ".",
#'   save_filename = "abx2014report",
#'   showprogress = TRUE
#' )
#' @seealso \code{\link{load_dataset}}, \code{\link{aggregate_data}}, \code{\link{check_dataset}}
#' @export
generate_report <- function(sourcedata, aggregatedata, save_directory = ".", save_filename = NULL, format = "html", showprogress = TRUE){

	log_function_start(match.call()[[1]])

	save_directory <- validate_param_dir(save_directory)
	if( is.null(save_filename) ){
		save_filename <- paste0("ehrchangepoints_report_", format(Sys.time(), "%Y%m%d%_%H%M%S"))
	} else{
		validate_param_savefilename(save_filename)
	}

	fileandpath <- file.path(save_directory, paste0(save_filename, ".html"))

	if( format == "html" ){
		log_message("Generating html report...", showprogress)
		rmarkdown::render(input = system.file("rmd", "report_htmldoc.Rmd", package = utils::packageName(), mustWork = TRUE)
											, output_file = paste0(save_filename, ".html")
											, output_dir = save_directory
											, params = list(sourcedata = sourcedata, aggregatedata = aggregatedata)
											, quiet = !showprogress)
	} else{
		stop(paste("Invalid format: ", format, ". Only html format is currently supported"))
	}

	log_message(paste0("Report saved to: ", fileandpath), showprogress)

	log_function_end(match.call()[[1]])

	fileandpath

}

# -----------------------------------------------------------------------------
# create an individual plot according to aggregatetype
# optionally plot changepoints - "all"/"none"/vectorofmethodnames
plot_timeseries_static <- function(aggfield, aggtype, changepoint_methods = "none"){
	#temp assignment
	# aggfield<-testcpddata_byday$aggregatefields[[3]]
	# aggfield<-testcpddata_byday$aggregatefields[[8]]$subaggregates[[1]][[1]]
	#   aggtype = "min"
	# changepoint_methods = "none"

	timepointcolname <- names(aggfield$values)[1]
	# set up universal plot characteristics
	g <- ggplot2::ggplot(aggfield$values[, c(timepointcolname, aggtype), with = FALSE], ggplot2::aes_string(timepointcolname, aggtype)) +
		ggplot2::scale_x_date(breaks = scales::breaks_pretty(12),
													labels = scales::label_date_short(sep = " "),
# breaks = "1 year", labels = scales::date_format("%Y")
													expand = c(0,0)) +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
		# no labels needed as info will be in the holding section
		ggplot2::labs(x = NULL, y = NULL, title = NULL)

	# if all values are NA, maxval will be infinite, so show a blank plot. So far only happens for min/max aggtypes
	maxval <- suppressWarnings(max(aggfield$values[[aggtype]], na.rm = TRUE))
	minval <- suppressWarnings(min(aggfield$values[[aggtype]], na.rm = TRUE))
	if( is.infinite(maxval) ){
		if( is.fieldtype_datetime(aggfield$fieldtype) ){
			g <- g + ggplot2::geom_blank(ggplot2::aes_string(x = timepointcolname, y = timepointcolname))
		} else if ( is.fieldtype_numeric(aggfield$fieldtype) ){
			# TODO: this isn't working yet
			g <- g + ggplot2::geom_blank(mapping = ggplot2::aes_string(x=timepointcolname, y="blankaggtype"), data = data.frame(aggfield$values[timepointcolname], blankaggtype=1))
		}
	} else{
		g <- g + ggplot2::geom_point(na.rm = TRUE, shape = 4)

		# specify y axis scale
		aggbreaks <- yscale_breaks(aggtype, maxval, minval, aggfield$fieldtype)
		if( !is.na(aggbreaks[1]) ){
			g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks, limits = c(aggbreaks[1], max(maxval, aggbreaks[length(aggbreaks)])))
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
	}

	g

}

# -----------------------------------------------------------------------------
# create a plot to show overall numbers per timepoint
# TODO: automatically choose to draw a lineplot or barplot depending on number of timepoints (as barplots don't render well with lots of timepoints)
plot_overview_totals_static <- function(aggfield, aggtype, fillcolour = NA, title = NULL){
	#temp assignment
	# aggfield <- testcpddata_byday$aggregatefields[[testcpddata_byday$timepoint_fieldname]]
	# aggtype = "n"
	# fillcolour = "pink"
	# aggfield <- testcpddata_byday2[["aggregatefields"]][["ALLFIELDSCOMBINED"]]
	# aggtype = "nonconformant_n"

	timepointcolname <- names(aggfield$values)[1]
	data <- aggfield$values[, c(timepointcolname, aggtype), with = FALSE]

	g <- ggplot2::ggplot(data, ggplot2::aes_string(timepointcolname, aggtype)) +
		ggplot2::scale_x_date(breaks = scales::breaks_pretty(12),
													labels = scales::label_date_short(sep = " "),
													# breaks = "1 year", labels = scales::date_format("%Y")
													expand = c(0,0)) +
		ggplot2::labs(y = NULL, x = NULL, title = title) +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.35, hjust = 1, size = 7),
									 axis.text.y = ggplot2::element_text(size = 7),
									 axis.title = ggplot2::element_text(size = 8),
									 plot.title = ggplot2::element_text(size=8, face = "bold", hjust = 0.5)) +
		ggplot2::labs(x = NULL, y = NULL, title = title)

	g <- g + ggplot2::geom_line(na.rm = TRUE) +
	# use ribbon instead of area so that NAs don't get interpolated
	ggplot2::geom_ribbon(data = data[!is.na(get(aggtype)), ymin := 0], ggplot2::aes_string(x = timepointcolname, ymin = "ymin", ymax = aggtype), fill = fillcolour, alpha = 0.5)

	# specify y axis scale
	maxval <- suppressWarnings(max(aggfield$values[[aggtype]], na.rm = TRUE))
	minval <- suppressWarnings(min(aggfield$values[[aggtype]], na.rm = TRUE))
	aggbreaks <- yscale_breaks(aggtype, maxval, minval, aggfield$fieldtype)
	if( !is.na(aggbreaks[1]) ){
		g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks, limits = c(aggbreaks[1], max(maxval, aggbreaks[length(aggbreaks)])))
	}

	g

}

# -----------------------------------------------------------------------------
# create a heatmap showing a particular aggtype value across all fields
# TODO: Decide whether or not to include the timepoint field in the heatmap
plot_overview_heatmap_static <- function(aggfields, aggtype, fillcolour = "darkred"){
	#temp assignment
	# aggfields<-testcpddata_byday$aggregatefields
	#   aggtype = "n"
	# fillcolour = "darkred"

	timepointcolname <- names(aggfields[[1]]$values)[1]

	# get aggtype values from each datafield
	heatmapfields <- names(aggfields)[which(!names(aggfields) %in% c("DUPLICATES", "ALLFIELDSCOMBINED"))]
	data <- data.table::data.table()
	for(i in seq_along(heatmapfields)){
		f <- heatmapfields[i]
		if( aggtype %in% names(aggfields[[f]]$values) ){
			d <- aggfields[[f]]$values[, c(timepointcolname, aggtype), with = FALSE]
			d[, fieldname := f]
		} else{
			d <- aggfields[[f]]$values[, timepointcolname, with = FALSE]
			d[, fieldname := f]
			d[, (aggtype) := NA_integer_]
		}
		data <- rbind(data, d)
	}
	data[, fieldname := factor(fieldname, levels = names(aggfields))]

	# when the only values are zero, make sure the fill colour is white (as geom_tile uses the 'high' colour)
	if( all(data[, aggtype, with = FALSE] == 0, na.rm = TRUE) ){
		fillcolour <- "white"
	}

	g <- ggplot2::ggplot(data, ggplot2::aes_string(timepointcolname, "fieldname", fill = aggtype)) +
		ggplot2::geom_tile() +
		ggplot2::scale_fill_gradient("Instances", low="white", high = fillcolour, na.value = "grey",
																 labels=NULL,
																 limits = c(0, NA)) +
		ggplot2::scale_x_date(breaks = scales::breaks_pretty(12),
													labels = scales::label_date_short(sep = " "),
													expand = c(0,0)) +
		ggplot2::labs(y = "Instances per fieldname", x = NULL) +
		#facet by variable (field name) to create separate bars
		ggplot2::facet_grid(fieldname~., scales = "free", space = "free") +
		ggplot2::theme_bw() +
		ggplot2::theme(#remove grid lines
			panel.grid.major = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			#remove facet labels and their background
			strip.background = ggplot2::element_blank(),
			strip.text.y = ggplot2::element_blank(),
			#add borders to the bars
			panel.border = ggplot2::element_rect(colour = "darkgrey", fill = NA, size = 0.75),
			#remove space between facets
			panel.spacing = ggplot2::unit(0, "lines"),
			#remove y-axis ticks
			axis.ticks.y = ggplot2::element_blank(),
			axis.title = ggplot2::element_text(size = 8),
			axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.35, hjust = 1, size = 7),
			axis.text.y = ggplot2::element_text(size = 7),
			legend.position = "none",
			# legend.key.size = ggplot2::unit(0.32,"cm"),
			# legend.justification = "top",
			# legend.title = ggplot2::element_text(size = 8, face = "bold"),
			# legend.text = ggplot2::element_text(size=7),
			# legend.background = ggplot2::element_rect(colour = "black", size = 0.25),
			)
	g
}

# -----------------------------------------------------------------------------
# combine a lineplot and heatmap to show as an overall summary for a particular aggtype
plot_overview_combo_static <- function(aggfields, aggtype, lineplot_fieldname, lineplot_fillcolour, heatmap_fillcolour, title = NULL){
	# aggfields = testcpddata_byday2$aggregatefields
	# aggtype = "nonconformant_n"
	# lineplot_fieldname = "ALLFIELDSCOMBINED"
	# lineplot_fillcolour = "lightgreen"
	# heatmap_fillcolour = "darkgreen"
	# title = "Total nonconformant values"

	totals <- plot_overview_totals_static(aggfield = aggfields[[lineplot_fieldname]],
																				aggtype = aggtype,
																				fillcolour = lineplot_fillcolour,
																				title = title)


	# TODO: Decide whether or not to include the timepoint field in the heatmap
	heatmap <- plot_overview_heatmap_static(aggfields = aggfields,
																					aggtype = aggtype,
																					fillcolour = heatmap_fillcolour)

	cowplot::plot_grid(plotlist = list(totals, heatmap), ncol = 1, align = "v", axis = "lr", rel_heights = c(1,3))

}

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS

yscale_breaks <- function(aggtype, maxval, minval = 0, fieldtype = NULL){
	breaks <- NA

	if( aggtype %in% c("distinct","n","sum") | endsWith(aggtype, "_n") | startsWith(aggtype, "subcat_n") ){
		rangesize <- floor(log10(maxval))
		if( rangesize == 0 || maxval == 0){
			breaks <- seq(0, max(maxval, 10))
		}
		else{
			ymax <- ceiling(maxval/10^(rangesize-1))*10^(rangesize-1)
			breaks <- seq(0, ymax, by = 10^rangesize)
		}
	} else if( endsWith(aggtype, "_perc") | startsWith(aggtype, "subcat_perc") ){
		breaks <- seq(0, 100, by = 10)
	} else if( aggtype %in% c("min","max") ){
		if( is.fieldtype_numeric(fieldtype) ){
			if( maxval < minval ){
				# TODO: maybe better to return a warning and/or a graph with the error written on it rather than stopping altogether
				stop(paste0("Invalid parameter(s) supplied: minval [", toString(minval),"] is greater than maxval [", toString(maxval),"]"), call. = FALSE)
			}
			rangesize <- floor(log10(maxval - minval))
			if( rangesize == 0 || (maxval == 0 && minval == 0) ) {
				breaks <- seq(floor(minval), max(ceiling(maxval), 5))
			}
			else{
				ymin <- floor(minval/10^(rangesize-1))*10^(rangesize-1)
				ymax <- ceiling(maxval/10^(rangesize-1))*10^(rangesize-1)
				breaks <- seq(ymin, ymax, by = 10^rangesize)
			}
		} else if( is.fieldtype_datetime(fieldtype) ){
			# date axis seems to magically render itself
		}
	}
	breaks
}

