# Functions for generating data/plots for reports

# -----------------------------------------------------------------------------
#' Generate report from existing objects
#'
#' Generate report from previously-created sourcedata and aggregatedata objects
#'
#' @param sourcedata A \code{sourcedata} object returned from
#'   \code{\link{prepare_data}} function
#' @param aggregatedata An \code{aggregatedata} object returned from
#'   \code{\link{aggregate_data}} function
#' @param save_directory String specifying directory in which to save the
#'   report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any
#'   file extension. If no filename is supplied, one will be automatically
#'   generated with the format daiquiri_report_YYMMDD_HHMMSS.
#' @param format File format of the report. Currently only "html" is supported
#' @param showprogress Print progress to console. Default = TRUE
#' @return A string containing the name and path of the saved report
#' @examples
#' \donttest{
#' # load example data into a data.frame
#' rawdata <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' # validate and prepare the data for aggregation
#' sourcedataobj <- prepare_data(
#'   rawdata,
#'   fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category=TRUE)),
#'   override_columnnames = FALSE,
#'   na = c("","NULL"),
#'   dataset_shortdesc = "Example data provided with package",
#'   showprogress = TRUE
#' )
#'
#' # aggregate the data
#' aggregatedataobj <- aggregate_data(
#'   sourcedataobj,
#'   aggregation_timeunit = "day",
#'   showprogress = TRUE
#' )
#'
#' # save a report in the current directory using the previously-created objects
#' report_data(
#'   sourcedataobj,
#'   aggregatedataobj,
#'   save_directory = ".",
#'   save_filename = "example_data_report",
#'   showprogress = TRUE
#' )
#'
#' \dontshow{file.remove("./example_data_report.html")}
#' }
#'
#' @seealso \code{\link{prepare_data}}, \code{\link{aggregate_data}},
#'   \code{\link{create_report}}
#' @export
report_data <- function(sourcedata,
												aggregatedata,
												save_directory = ".",
												save_filename = NULL,
												format = "html",
												showprogress = TRUE) {

	log_function_start(match.call()[[1]])

	validate_params_required(match.call())
	validate_params_type(match.call(),
											 sourcedata = sourcedata,
											 aggregatedata = aggregatedata,
											 save_directory = save_directory,
											 save_filename = save_filename,
											 showprogress = showprogress,
											 format = format)

	if (is.null(save_filename)) {
		save_filename <-
			paste0("daiquiri_report_", format(Sys.time(), "%Y%m%d%_%H%M%S"))
	}

	fileandpath <- file.path(save_directory, paste0(save_filename, ".html"))

	if (format == "html") {
		log_message("Generating html report...", showprogress)
		rmarkdown::render(
			input = system.file(
								"rmd",
								"report_htmldoc.Rmd",
								package = utils::packageName(),
								mustWork = TRUE
							),
			output_file = paste0(save_filename, ".html"),
			output_dir = save_directory,
			params = list(sourcedata = sourcedata, aggregatedata = aggregatedata),
			quiet = !showprogress
		)
	} else{
		stop(paste(
			"Invalid format: ", format,
			". Only html format is currently supported"
		))
	}

	log_message(paste0("Report saved to: ", fileandpath), showprogress)

	log_function_end(match.call()[[1]])

	fileandpath

}

# -----------------------------------------------------------------------------
#' Create a scatter plot for an individual time series
#'
#' @param aggfield aggregatefield object
#' @param aggtype string denoting aggregatetype (from aggfield columnname)
#' @return ggplot
#' @noRd
plot_timeseries_static <- function(aggfield,
																	 aggtype) {

	timepointcolname <- names(aggfield$values)[1]
	# set up universal plot characteristics
	g <-
		ggplot2::ggplot(aggfield$values[, c(timepointcolname, aggtype), with = FALSE],
										ggplot2::aes_string(timepointcolname, aggtype)) +
		ggplot2::scale_x_date(
			breaks = scales::breaks_pretty(12),
			labels = scales::label_date_short(sep = " "),
			# breaks = "1 year", labels = scales::date_format("%Y")
			expand = c(0, 0)
		) +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
		# no labels needed as info will be in the holding section
		ggplot2::labs(
			x = NULL,
			y = paste0(
				aggtype_friendlyname(aggtype, "long"),
				ifelse(
					aggfield$columnname == "[DUPLICATES]",
					"",
					paste0("\n(", aggfield$columnname, ")")
				)
			),
			title = NULL
		)

	# if all values are NA, show a blank plot, otherwise plot the values
	if (!all(is.na(aggfield$values[[aggtype]]))) {
		g <- g + ggplot2::geom_point(na.rm = TRUE, shape = 4)

		# specify y axis scale
		maxval <- max(aggfield$values[[aggtype]], na.rm = TRUE)
		minval <- min(aggfield$values[[aggtype]], na.rm = TRUE)
		aggbreaks <-
			yscale_breaks(aggtype, maxval, minval, aggfield$fieldtype)
		g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks,
																				 limits = c(min(minval, aggbreaks[1]),
																				 					 max(maxval, aggbreaks[length(aggbreaks)]))
																				 )

	}

	g

}

# -----------------------------------------------------------------------------
#' Create a filled line plot to show overall numbers per timepoint
#'
#' @param aggfield aggregatefield object
#' @param aggtype string denoting aggregatetype (from aggfield columnname)
#' @param fillcolour colour to use below the line
#' @param title optional title for the plot
#' @return ggplot
#' @noRd
# TODO: automatically choose to draw a lineplot or barplot depending on number of timepoints (as barplots don't render well with lots of timepoints)
plot_overview_totals_static <- function(aggfield,
																				aggtype,
																				fillcolour = NA,
																				title = NULL) {

	# initialise known column names to prevent R CMD check notes
	ymin <- NULL

	timepointcolname <- names(aggfield$values)[1]
	data <-
		aggfield$values[, c(timepointcolname, aggtype), with = FALSE]

	g <-
		ggplot2::ggplot(data, ggplot2::aes_string(timepointcolname, aggtype)) +
		ggplot2::scale_x_date(
			breaks = scales::breaks_pretty(12),
			labels = scales::label_date_short(sep = " "),
			# breaks = "1 year", labels = scales::date_format("%Y")
			expand = c(0, 0)
		) +
		ggplot2::labs(y = NULL, x = NULL, title = title) +
		ggplot2::theme_bw() +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(
				angle = 90,
				vjust = 0.35,
				hjust = 1,
				size = 7
			),
			axis.text.y = ggplot2::element_text(size = 7),
			axis.title = ggplot2::element_text(size = 8),
			plot.title = ggplot2::element_text(size = 8, face = "bold", hjust = 0.5)
		) +
		ggplot2::labs(x = NULL, y = NULL, title = title)

	# if all values are NA, show a blank plot, otherwise plot the values
	if (!all(is.na(aggfield$values[[aggtype]]))) {
		g <- g + ggplot2::geom_line(na.rm = TRUE) +
			# use ribbon instead of area so that NAs don't get interpolated
			ggplot2::geom_ribbon(
				data = data[!is.na(get(aggtype)), ymin := 0],
				ggplot2::aes_string(x = timepointcolname, ymin = "ymin", ymax = aggtype),
				fill = fillcolour,
				alpha = 0.5
			)

		# specify y axis scale
		maxval <- max(aggfield$values[[aggtype]], na.rm = TRUE)
		g <- g + ggplot2::scale_y_continuous(n.breaks = 6,
																				 limits = c(0, max(maxval, 10)))
	}

	g

}

# -----------------------------------------------------------------------------
#' Create a heatmap showing a particular aggtype value across all fields
#'
#' @param aggfields all aggregatefields object
#' @param aggtype string denoting aggregatetype (from aggfield columnname)
#' @param fillcolour colour to use for the tiles
#' @return ggplot
#' @noRd
# TODO: Decide whether or not to include the timepoint field in the heatmap
plot_overview_heatmap_static <- function(aggfields,
																				 aggtype,
																				 fillcolour = "darkred") {

	# initialise known column names to prevent R CMD check notes
	fieldname <- NULL

	timepointcolname <- names(aggfields[[1]]$values)[1]

	# get aggtype values from each datafield
	heatmapfields <-
		names(aggfields)[which(!names(aggfields) %in% c("[DUPLICATES]", "[ALLFIELDSCOMBINED]"))]
	data <- data.table::data.table()
	for (i in seq_along(heatmapfields)) {
		f <- heatmapfields[i]
		if (aggtype %in% names(aggfields[[f]]$values)) {
			d <-
				aggfields[[f]]$values[, c(timepointcolname, aggtype), with = FALSE]
			d[, fieldname := f]
		} else{
			d <- aggfields[[f]]$values[, timepointcolname, with = FALSE]
			d[, fieldname := f]
			d[, (aggtype) := NA_integer_]
		}
		data <- rbind(data, d)
	}
	data[, fieldname := factor(fieldname, levels = names(aggfields))]

	# when the only values are zero, make sure the fill colour is white (as
	# geom_tile uses the 'high' colour)
	if (all(data[, aggtype, with = FALSE] == 0, na.rm = TRUE)) {
		fillcolour <- "white"
	}

	g <-
		ggplot2::ggplot(data,
										ggplot2::aes_string(timepointcolname, "fieldname", fill = aggtype)) +
		ggplot2::geom_tile() +
		ggplot2::scale_fill_gradient(
			"Instances",
			low = "white",
			high = fillcolour,
			na.value = "grey",
			labels = NULL,
			limits = c(0, NA)
		) +
		ggplot2::scale_x_date(
			breaks = scales::breaks_pretty(12),
			labels = scales::label_date_short(sep = " "),
			expand = c(0, 0)
		) +
		ggplot2::labs(y = "Instances per fieldname", x = NULL) +
		#facet by variable (field name) to create separate bars
		ggplot2::facet_grid(fieldname ~ ., scales = "free", space = "free") +
		ggplot2::theme_bw() +
		ggplot2::theme(
			#remove grid lines
			panel.grid.major = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			#remove facet labels and their background
			strip.background = ggplot2::element_blank(),
			strip.text.y = ggplot2::element_blank(),
			#add borders to the bars
			panel.border = ggplot2::element_rect(
				colour = "darkgrey",
				fill = NA,
				size = 0.75
			),
			#remove space between facets
			panel.spacing = ggplot2::unit(0, "lines"),
			#remove y-axis ticks
			axis.ticks.y = ggplot2::element_blank(),
			axis.title = ggplot2::element_text(size = 8),
			axis.text.x = ggplot2::element_text(
				angle = 90,
				vjust = 0.35,
				hjust = 1,
				size = 7
			),
			axis.text.y = ggplot2::element_text(size = 7),
			legend.position = "none",
		)

	g

}

# -----------------------------------------------------------------------------
#' Combine a lineplot and heatmap to show as an overall summary for a particular
#' aggtype
#'
#' @param aggfields all aggregatefields to be included
#' @param aggtype string denoting aggregatetype (from aggfield columnname)
#' @param lineplot_fieldname which aggfield to use for the lineplot
#' @param lineplot_fillcolour colour to use below the line
#' @param heatmap_fillcolour colour to use for the tiles
#' @param title optional title for the combined plot
#' @return cowplot::plot_grid
#' @noRd
plot_overview_combo_static <- function(aggfields,
																			 aggtype,
																			 lineplot_fieldname,
																			 lineplot_fillcolour,
																			 heatmap_fillcolour,
																			 title = NULL) {

	totals <-
		plot_overview_totals_static(
			aggfield = aggfields[[lineplot_fieldname]],
			aggtype = aggtype,
			fillcolour = lineplot_fillcolour,
			title = title
		)

	# TODO: Decide whether or not to include the timepoint field in the heatmap
	heatmap <- plot_overview_heatmap_static(aggfields = aggfields,
																					aggtype = aggtype,
																					fillcolour = heatmap_fillcolour)

	cowplot::plot_grid(
		plotlist = list(totals, heatmap),
		ncol = 1,
		align = "v",
		axis = "lr",
		rel_heights = c(1, 3)
	)

}

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS

#' Set the breaks for the y-axis depending on the fieldtype and aggtype
#'
#' @param aggtype string denoting aggregatetype (from aggfield columnname)
#' @param maxval maximum data value
#' @param minval minimum data value
#' @param fieldtype fieldtype object
#' @return numeric vector containing locations of limits and breaks
#' @noRd
yscale_breaks <- function(aggtype,
													maxval,
													minval = 0,
													fieldtype = NULL) {

	breaks <- NULL

	if (aggtype %in% c("distinct", "n", "sum", "minlength", "maxlength", "meanlength")
			|| endsWith(aggtype, "_n") || startsWith(aggtype, "subcat_n")) {
		# frequency/length aggtypes should always start at zero and be shown on a
		# range of 0-10 at a minimum
		if (maxval <= 10) {
			breaks <- seq(0, 10)
		}
		else{
			breaks <- pretty(c(0, maxval))
		}
	} else if (endsWith(aggtype, "_perc") ||
						 startsWith(aggtype, "subcat_perc")) {
		# percentage aggtypes should always be shown on a range of 0-100
		breaks <- seq(0, 100, by = 10)
	} else{
		if (is.fieldtype_datetime(fieldtype)) {
			# dates should be left to base
			breaks <- pretty(c(minval, maxval))
		} else{
			# otherwise set range based on min/max values
			if (maxval == minval) {
				# if all values are the same, plot them somewhere in the middle
				breaks <- seq(floor(minval - 1), ceiling(maxval + 1))
			}
			else{
				breaks <- pretty(c(minval, maxval))
			}
		}
	}

	breaks

}
