# Functions for generating data/plots for reports

# -----------------------------------------------------------------------------
#' Generate report from existing objects
#'
#' Generate report from previously-created `daiquiri_source_data` and
#' `daiquiri_aggregated_data` objects
#'
#' @param source_data A `daiquiri_source_data` object returned from
#'   [prepare_data()] function
#' @param aggregated_data A `daiquiri_aggregated_data` object returned from
#'   [aggregate_data()] function
#' @param report_title Title to appear on the report
#' @param save_directory String specifying directory in which to save the
#'   report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any
#'   file extension. If no filename is supplied, one will be automatically
#'   generated with the format `daiquiri_report_YYMMDD_HHMMSS`.
#' @param format File format of the report. Currently only `"html"` is supported
#' @param show_progress Print progress to console. Default = `TRUE`
#' @param ... Further parameters to be passed to `rmarkdown::render()`. Cannot
#'   include any of `input`, `output_dir`, `output_file`, `params`, `quiet`.
#' @return A string containing the name and path of the saved report
#' @examples
#' \donttest{
#' # load example data into a data.frame
#' raw_data <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' # validate and prepare the data for aggregation
#' source_data <- prepare_data(
#'   raw_data,
#'   field_types = field_types(
#'     PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category = TRUE)
#'   ),
#'   override_column_names = FALSE,
#'   na = c("", "NULL"),
#'   dataset_description = "Example data provided with package",
#'   show_progress = TRUE
#' )
#'
#' # aggregate the data
#' aggregated_data <- aggregate_data(
#'   source_data,
#'   aggregation_timeunit = "day",
#'   show_progress = TRUE
#' )
#'
#' # save a report in the current directory using the previously-created objects
#' report_data(
#'   source_data,
#'   aggregated_data,
#'   report_title = "daiquiri data quality report",
#'   save_directory = ".",
#'   save_filename = "example_data_report",
#'   show_progress = TRUE
#' )
#' \dontshow{file.remove("./example_data_report.html")}
#' }
#'
#' @seealso [prepare_data()], [aggregate_data()],
#'   [daiquiri_report()]
#' @export
report_data <- function(source_data,
                        aggregated_data,
                        report_title = "daiquiri data quality report",
                        save_directory = ".",
                        save_filename = NULL,
                        format = "html",
                        show_progress = TRUE,
                        ...) {
  log_function_start(match.call()[[1]])

  validate_params_required(match.call())
  validate_params_type(match.call(),
    source_data = source_data,
    aggregated_data = aggregated_data,
    report_title = report_title,
    save_directory = save_directory,
    save_filename = save_filename,
    show_progress = show_progress,
    format = format
  )

  timestamp_string <- format(Sys.time(), "%Y%m%d%_%H%M%S")

  if (is.null(save_filename)) {
    save_filename <-
      paste0("daiquiri_report_", timestamp_string)
  }

  file_and_path <- file.path(save_directory, paste0(save_filename, ".html"))

  # temporarily copy rmd file from package library into save_directory so that
  # intermediate files also get created there.
  # NOTE: explicitly setting intermediates_dir in rmarkdown::render() to
  # save_directory or tempdir() causes duplicate chunk label errors when package
  # is run from inside an rmd/qmd
  temp_dirname <-
    file.path(save_directory, paste0("daiquiri_temp_", timestamp_string))
  dir.create(temp_dirname)
  file.copy(
    from = system.file(
      "rmd",
      "report_htmldoc.Rmd",
      package = utils::packageName(),
      mustWork = TRUE
    ),
    to = temp_dirname,
    overwrite = TRUE
  )

  if (format == "html") {
    log_message("Generating html report...", show_progress)
    rmarkdown::render(
      input = file.path(temp_dirname,
                        "report_htmldoc.Rmd"),
      output_file = paste0(save_filename, ".html"),
      output_dir = save_directory,
      params = list(
        source_data = source_data,
        aggregated_data = aggregated_data,
        report_title = report_title
      ),
      quiet = !show_progress,
      ...
    )
  } else {
    stop(paste(
      "Invalid format: ", format,
      ". Only html format is currently supported"
    ))
  }

  log_message(paste0("Report saved to: ", file_and_path), show_progress)

  # remove temporary directory created earlier
  unlink(temp_dirname, recursive = TRUE)

  log_function_end(match.call()[[1]])

  file_and_path
}


# -----------------------------------------------------------------------------
#' Create a scatter plot for an individual time series
#'
#' @param agg_field aggregated_field object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @return ggplot
#' @noRd
#' @importFrom ggplot2 .data
plot_timeseries_static <- function(agg_field,
                                   agg_fun) {
  timepoint_aggcol_name <- names(agg_field$values)[1]
  # set up universal plot characteristics
  g <-
    ggplot2::ggplot(
      agg_field$values[, c(timepoint_aggcol_name, agg_fun), with = FALSE],
      ggplot2::aes(.data[[timepoint_aggcol_name]], .data[[agg_fun]])
    ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    # no labels needed as info will be in the holding section
    ggplot2::labs(
      x = NULL,
      y = paste0(
        agg_fun_friendly_name(agg_fun, "long"),
        ifelse(
          agg_field$column_name == "[DUPLICATES]",
          "",
          paste0("\n(", agg_field$column_name, ")")
        )
      ),
      title = NULL
    )

  # if all values are NA, show a blank plot, otherwise plot the values
  if (!all(is.na(agg_field$values[[agg_fun]]))) {
    g <- g + ggplot2::geom_point(na.rm = TRUE, shape = 4)

    # specify y axis scale
    max_val <- max(agg_field$values[[agg_fun]], na.rm = TRUE)
    min_val <- min(agg_field$values[[agg_fun]], na.rm = TRUE)
    y_breaks <-
      yscale_breaks(agg_fun, max_val, min_val, agg_field$field_type)
    g <- g + ggplot2::scale_y_continuous(
      breaks = y_breaks,
      limits = c(
        min(min_val, y_breaks[1]),
        max(max_val, y_breaks[length(y_breaks)])
      )
    )
  }

  g
}


# -----------------------------------------------------------------------------
#' Create a filled line plot to show overall numbers per timepoint
#'
#' @param agg_field aggregated_field object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @param fill_colour colour to use below the line
#' @param title optional title for the plot
#' @param stratum stratify_by value to optionally filter on
#' @return ggplot
#' @noRd
# TODO: automatically choose to draw a lineplot or barplot depending on number
# of timepoints (as barplots don't render well with lots of timepoints)
plot_overview_totals_static <- function(agg_field,
                                        agg_fun,
                                        fill_colour = NA,
                                        title = NULL,
                                        stratum = NULL) {
  # initialise known column names to prevent R CMD check notes
  ymin <- NULL

  timepoint_aggcol_name <- names(agg_field$values)[1]
  if (!is.null(stratum)) {
    stratify_aggcol_name <- names(agg_field$values)[2]
    if (is.na(stratum)) {
      data <- agg_field$values[is.na(get(stratify_aggcol_name)), c(timepoint_aggcol_name, agg_fun), with = FALSE]
    } else {
      data <- agg_field$values[get(stratify_aggcol_name) == stratum, c(timepoint_aggcol_name, agg_fun), with = FALSE]
    }
  } else {
    data <- agg_field$values[, c(timepoint_aggcol_name, agg_fun), with = FALSE]
  }

  g <-
    ggplot2::ggplot(data, ggplot2::aes(.data[[timepoint_aggcol_name]], .data[[agg_fun]])) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
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
  if (!all(is.na(agg_field$values[[agg_fun]]))) {
    g <- g + ggplot2::geom_line(na.rm = TRUE) +
      # use ribbon instead of area so that NAs don't get interpolated
      ggplot2::geom_ribbon(
        data = data[!is.na(get(agg_fun)), ymin := 0],
        ggplot2::aes(
          x = .data[[timepoint_aggcol_name]],
          ymin = ymin,
          ymax = .data[[agg_fun]]
        ),
        fill = fill_colour,
        alpha = 0.5
      )

    # specify y axis scale
    max_val <- max(agg_field$values[[agg_fun]], na.rm = TRUE)
    g <- g + ggplot2::scale_y_continuous(
      n.breaks = 6,
      limits = c(0, max(max_val, 10))
    )
  }

  g
}


# -----------------------------------------------------------------------------
#' Create a heatmap showing a particular agg_fun value across all fields
#'
#' @param agg_fields all aggregated_fields or aggregated_fields_stratified object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @param fill_colour colour to use for the tiles
#' @param stratum stratify_by value to optionally filter on
#' @return ggplot
#' @noRd
# TODO: Decide whether or not to include the timepoint field in the heatmap
plot_overview_heatmap_static <- function(agg_fields,
                                         agg_fun,
                                         fill_colour = "darkred",
                                         stratum = NULL) {
  # initialise known column names to prevent R CMD check notes
  field_name <- NULL

  timepoint_aggcol_name <- names(agg_fields[[1]]$values)[1]
  if (!is.null(stratum)) {
    stratify_aggcol_name <- names(agg_fields[[1]]$values)[2]
  }

  # get agg_fun values from each data_field
  heatmap_fields <-
    names(agg_fields)[which(!names(agg_fields) %in% c("[DUPLICATES]", "[ALL_FIELDS_COMBINED]"))]
  data <- data.table::data.table()
  for (i in seq_along(heatmap_fields)) {
    f <- heatmap_fields[i]
    if (!is.null(stratum)) {
      if (is.na(stratum)) {
        aggdata <- agg_fields[[f]]$values[is.na(get(stratify_aggcol_name))]
      } else {
        aggdata <- agg_fields[[f]]$values[get(stratify_aggcol_name) == stratum]
      }
    } else {
      aggdata <- agg_fields[[f]]$values
    }
    if (agg_fun %in% names(agg_fields[[f]]$values)) {
      d <- aggdata[, c(timepoint_aggcol_name, agg_fun), with = FALSE]
      d[, field_name := f]
    } else {
      d <- aggdata[, timepoint_aggcol_name, with = FALSE]
      d[, field_name := f]
      d[, (agg_fun) := NA_integer_]
    }
    data <- rbind(data, d)
  }
  data[, field_name := factor(field_name, levels = names(agg_fields))]

  # when the only values are zero, make sure the fill colour is white (as
  # geom_tile uses the 'high' colour)
  if (all(data[, agg_fun, with = FALSE] == 0, na.rm = TRUE)) {
    fill_colour <- "white"
  }

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(.data[[timepoint_aggcol_name]], field_name, fill = .data[[agg_fun]])
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      "Instances",
      low = "white",
      high = fill_colour,
      na.value = "grey",
      labels = NULL,
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::labs(y = "Instances per field", x = NULL) +
    # facet by variable (field name) to create separate bars
    ggplot2::facet_grid(field_name ~ ., scales = "free", space = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # remove facet labels and their background
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        size = 0.75
      ),
      # remove space between facets
      panel.spacing = ggplot2::unit(0, "lines"),
      # remove y-axis ticks
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
#' aggregation function
#'
#' @param agg_fields all aggregated_fields to be included
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @param lineplot_field_name which aggregated_field to use for the lineplot
#' @param lineplot_fill_colour colour to use below the line
#' @param heatmap_fill_colour colour to use for the tiles
#' @param title optional title for the combined plot
#' @param stratum stratify_by value to optionally filter on
#' @return cowplot::plot_grid
#' @noRd
plot_overview_combo_static <- function(agg_fields,
                                       agg_fun,
                                       lineplot_field_name,
                                       lineplot_fill_colour,
                                       heatmap_fill_colour,
                                       title = NULL,
                                       stratum = NULL) {
  totals <-
    plot_overview_totals_static(
      agg_field = agg_fields[[lineplot_field_name]],
      agg_fun = agg_fun,
      fill_colour = lineplot_fill_colour,
      title = title,
      stratum = stratum
    )

  # TODO: Decide whether or not to include the timepoint field in the heatmap
  heatmap <- plot_overview_heatmap_static(
    agg_fields = agg_fields,
    agg_fun = agg_fun,
    fill_colour = heatmap_fill_colour,
    stratum = stratum
  )


  suppressMessages(
    cowplot::plot_grid(
      plotlist = list(totals, heatmap),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 3)
    )
  )
}


# -----------------------------------------------------------------------------
#' Create a heatmap showing value across all subcategories
#'
#' @param agg_field categorical aggfield object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   function_list)
#' @return ggplot
#' @noRd
plot_subcat_heatmap_static <- function(agg_field,
                                         agg_fun) {
  # initialise known column names to prevent R CMD check notes
  field_name <- NULL

  agg_field_values <- agg_field[1]$values
  agg_field_cols <- names(agg_field_values)
  timepoint_aggcol_name <- agg_field_cols[1]

  # get agg_fun values from each subcategory
  heatmap_fields <-
    agg_field_cols[which(startsWith(agg_field_cols, agg_fun))]
  data <- data.table::data.table()
  for (i in seq_along(heatmap_fields)) {
    f <- heatmap_fields[i]
    d <-
      agg_field_values[, c(timepoint_aggcol_name, f), with = FALSE]
    names(d)[2] <- agg_fun
    d[, field_name := agg_fun_subcat_value(f)]
    data <- rbind(data, d)
  }
  data[, field_name := factor(field_name, levels = agg_fun_subcat_value(heatmap_fields))]

  # when the only values are zero, make sure the fill colour is white (as
  # geom_tile uses the 'high' colour)
  if (all(data[, agg_fun, with = FALSE] == 0, na.rm = TRUE)) {
    fill_colour <- "white"
  }

  fill_colour <- switch(agg_fun,
                        "subcat_n" = "chocolate4",
                        "subcat_perc" = "darkorchid4")
  y_lab_prefix <- switch(agg_fun,
                         "subcat_n" = "No. of values per category",
                         "subcat_perc" = "Percentage of values per category")

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(.data[[timepoint_aggcol_name]], field_name, fill = .data[[agg_fun]])
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = fill_colour,
      na.value = "grey",
      labels = NULL,
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::labs(y = paste0(y_lab_prefix, "\n(", agg_field$column_name, ")"), x = NULL) +
    # facet by variable (field name) to create separate bars
    ggplot2::facet_grid(field_name ~ ., scales = "free", space = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # remove facet labels and their background
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        size = 0.75
      ),
      # remove space between facets
      panel.spacing = ggplot2::unit(0, "lines"),
      # remove y-axis ticks
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
#' Create a grid of scatter plots showing value across all strata
#'
#' @param agg_field_stratified single aggregated_field_stratified object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @return ggplot
#' @noRd
plot_stratified_facetgrid_static <- function(agg_field_stratified,
                                           aggregation_function) {

  timepoint_aggcol_name <- names(agg_field_stratified$values)[1]
  stratify_aggcol_name <- names(agg_field_stratified$values)[2]

  # get values for the heatmap rows
  stratum_names <- unique(agg_field_stratified$values[, get(stratify_aggcol_name)])

  data <- agg_field_stratified$values
  data[, (stratify_aggcol_name) := factor(get(stratify_aggcol_name), levels = stratum_names)]

  # specify shared y axis scale based on min/max values across all strata
  max_val <- max(data[[aggregation_function]], na.rm = TRUE)
  min_val <- min(data[[aggregation_function]], na.rm = TRUE)
  y_breaks <- yscale_breaks(
    agg_fun = aggregation_function,
    max_val = max_val,
    min_val = min_val,
    compact = TRUE,
    field_type = agg_field_stratified$field_type)

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(.data[[timepoint_aggcol_name]], .data[[aggregation_function]])
    ) +
    ggplot2::geom_point(na.rm = TRUE, shape = 4, size = 0.5) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = y_breaks,
      limits = c(
        min(min_val, y_breaks[1]),
        max(max_val, y_breaks[length(y_breaks)])
      )
    ) +
    ggplot2::labs(
      y = NULL,
      x = NULL,
      title = paste0("Stratified by: ", stratify_aggcol_name)) +
    # facet by strata to create separate bars
    ggplot2::facet_grid(get(stratify_aggcol_name) ~ ., switch = "y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.minor = ggplot2::element_blank(),
      # format facet labels
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        size = 0.75
      ),
      # set space between facets
      panel.spacing = ggplot2::unit(0.5, "lines"),
      axis.title = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.35,
        hjust = 1,
        size = 7
      ),
      axis.text.y = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 8, face = "bold", hjust = 0.5),
      legend.position = "none",
    )

  g
}


# -----------------------------------------------------------------------------
#' Create a scatter plot to show overall values above stratified plot
#'
#' @param agg_field aggregated_field object
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @param title optional title for the plot
#' @return ggplot
#' @noRd
plot_stratified_totals_static <- function(agg_field,
                                        agg_fun,
                                        title = NULL) {

  timepoint_aggcol_name <- names(agg_field$values)[1]

  g <-
    ggplot2::ggplot(
      agg_field$values[, c(timepoint_aggcol_name, agg_fun), with = FALSE],
      ggplot2::aes(.data[[timepoint_aggcol_name]], .data[[agg_fun]])) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
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
  if (!all(is.na(agg_field$values[[agg_fun]]))) {
    g <- g + ggplot2::geom_point(na.rm = TRUE, shape = 4, size = 0.5)

    # specify y axis scale
    max_val <- max(agg_field$values[[agg_fun]], na.rm = TRUE)
    min_val <- min(agg_field$values[[agg_fun]], na.rm = TRUE)
    y_breaks <-
      yscale_breaks(agg_fun, max_val, min_val, compact = TRUE, agg_field$field_type)
    g <- g + ggplot2::scale_y_continuous(
      breaks = y_breaks,
      limits = c(
        min(min_val, y_breaks[1]),
        max(max_val, y_breaks[length(y_breaks)])
      )
    )
  }

  g

}


# -----------------------------------------------------------------------------
#' Combine overall and stratified scatterplots to show a summary for a particular
#' stratified data field and aggregation function
#'
#' @param agg_field aggregated_field to be included
#' @param agg_field_strat aggregated_field_stratified to be included
#' @param agg_fun which aggregation function to plot (from agg_field
#'   column_name)
#' @return cowplot::plot_grid
#' @noRd
plot_stratified_combo_static <- function(agg_field,
                                         agg_field_strat,
                                         agg_fun) {
  totals <-
    plot_stratified_totals_static(
      agg_field = agg_field,
      agg_fun = agg_fun,
      title = paste0(
        agg_fun_friendly_name(agg_fun, "long"),
        " (", agg_field_strat$column_name, ")"
        )
    )

  stratified <- plot_stratified_facetgrid_static(
    agg_field_stratified = agg_field_strat,
    aggregation_function = agg_fun
  )


  cowplot::plot_grid(
    plotlist = list(totals, stratified),
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(2,
                    length(unique(agg_field_strat$values[, get(agg_field_strat$stratify_by_field_name)]))
                  )
  )
}


# -----------------------------------------------------------------------------
#' Set the breaks for the y-axis depending on the field_type and agg_fun
#'
#' @param agg_fun aggregation function being plotted (from agg_field
#'   column_name)
#' @param max_val maximum data value
#' @param min_val minimum data value
#' @param field_type field_type object
#' @param compact return fewer breaks when graph will be small
#' @return numeric vector containing locations of limits and breaks
#' @noRd
yscale_breaks <- function(agg_fun,
                          max_val,
                          min_val = 0,
                          field_type = NULL,
                          compact = FALSE) {
  breaks <- NULL

  if (agg_fun %in% c("distinct", "n", "sum", "min_length", "max_length", "mean_length") ||
    endsWith(agg_fun, "_n") || startsWith(agg_fun, "subcat_n")) {
    # frequency/length agg_funs should always start at zero and be shown on a
    # range of 0-10 at a minimum
    if (max_val <= 10) {
      if (compact){
        breaks <- seq(0, 10, by = 2)
      } else{
        breaks <- seq(0, 10)
      }
    } else {
      breaks <- pretty(c(0, max_val))
    }
  } else if (endsWith(agg_fun, "_perc") ||
    startsWith(agg_fun, "subcat_perc")) {
    # percentage agg_funs should always be shown on a range of 0-100
    if (compact){
      breaks <- seq(0, 100, by = 20)
    } else{
      breaks <- seq(0, 100, by = 10)
    }
  } else {
    if (is_ft_datetime(field_type)) {
      # dates should be left to base
      breaks <- pretty(c(min_val, max_val))
    } else {
      # otherwise set range based on min/max values
      if (max_val == min_val) {
        # if all values are the same, plot them somewhere in the middle
        breaks <- seq(floor(min_val - 1), ceiling(max_val + 1))
      } else {
        breaks <- pretty(c(min_val, max_val))
      }
    }
  }

  breaks
}
