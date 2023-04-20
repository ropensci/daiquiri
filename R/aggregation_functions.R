# Create specifications for aggregation functions

# -----------------------------------------------------------------------------
#' Common constructor for agg_fun objects
#'
#' @param type string denoting the agg_fun type
#' @param function_call aggregation function to apply to the data values
#' @param value_if_no_records value to use for timepoints that contain no records. Only need to supply if not NA.
#' @param friendly_name_short for things like tabs
#' @param friendly_name_long for things like y-axis labels
#'
#' @return agg_fun object
#' @noRd
agg_fun <- function(type,
                    function_call,
                    value_if_no_records = NA,
                    friendly_name_short,
                    friendly_name_long) {
  structure(
    list(
      type = type,
      function_call = function_call,
      value_if_no_records = value_if_no_records,
      friendly_name_short = friendly_name_short,
      friendly_name_long = friendly_name_long
    )
  )
}

# -----------------------------------------------------------------------------
# Types of aggregation functions available for specification

#' n
#'
#' number of values missing (excludes non-conformant ones)
#' For timepoints with no records, set n to 0 (but all other aggregation_functions should show NA)
#' @return agg_fun object
#' @noRd
agg_fun_n <- function(){
  agg_fun(type = "n",
          function_call = quote(
            sum(!(is.na(values) & !is.nan(values)), na.rm = TRUE)
            ),
          value_if_no_records = 0,
          friendly_name_short = "n",
          friendly_name_long = "No. of values present"
          )
}

#' missing_n
#'
#' number of values missing (excludes non-conformant ones)
#' For timepoints with no records, value should show NA
#' @return agg_fun object
#' @noRd
agg_fun_missing_n <- function(){
  agg_fun(type = "missing_n",
          function_call = quote(
            sum(is.na(values) & !is.nan(values))
            ),
          friendly_name_short = "missing_n",
          friendly_name_long = "No. of missing values"
          )
}

#' missing_perc
#'
#' percentage of values missing (excludes non-conformant ones) out of number of records
#' For timepoints with no records, value should show NA
#' @return agg_fun object
#' @noRd
agg_fun_missing_perc <- function(){
  agg_fun(type = "missing_perc",
          function_call = quote(
            100 * sum(is.na(values) & !is.nan(values)) / length(values)
            ),
          friendly_name_short = "missing_perc",
          friendly_name_long = "Percentage of missing values"
          )
}

#' nonconformant_n
#'
#' number of nonconformant values
#' For timepoints with no records, value should show NA
#' @return agg_fun object
#' @noRd
agg_fun_nonconformant_n <- function(){
  agg_fun(type = "nonconformant_n",
          function_call = quote(
            sum(is.nan(values))
            ),
          friendly_name_short = "nonconformant_n",
          friendly_name_long = "No. of nonconformant values"
          )
}

#' nonconformant_perc
#'
#' percentage of nonconformant values out of number of records
#' For timepoints with no records, value should show NA
#' TODO: should the denominator be all rows or only conformant/nonmissing rows?
#'
#' @return agg_fun object
#' @noRd
agg_fun_nonconformant_perc <- function(){
  agg_fun(type = "nonconformant_perc",
          function_call = quote(
            100 * sum(is.nan(values)) / length(values)
            ),
          friendly_name_short = "nonconformant_perc",
          friendly_name_long = "Percentage of nonconformant values"
          )
}

#' sum
#'
#' sum of values (used to indicate number of duplicate rows removed)
#' For timepoints with no records, value should show NA
#'
#' @return agg_fun object
#' @noRd
agg_fun_sum <- function(){
  agg_fun(type = "sum",
          function_call = quote(
            sum(values, na.rm = TRUE)
            ),
          friendly_name_short = "sum",
          friendly_name_long = "No. of duplicate records removed"
          )
}

#' nonzero_perc
#'
#' percentage of values which are non-zero out of number of values present
#' (used to indicate percentage of remaining records that were duplicated)
#'
#' @return agg_fun object
#' @noRd
agg_fun_nonzero_perc <- function(){
  agg_fun(type = "nonzero_perc",
          function_call = quote(
            100 * length(which(values > 0)) / length(values[!is.na(values)])
            ),
          friendly_name_short = "nonzero_perc",
          friendly_name_long = "Percentage of (remaining) records that were duplicated"
          )
}

#' distinct
#'
#' number of distinct values (excluding NAs)
#' For timepoints with no non-missing values, value should show NA for
#'   consistency with other fieldtype-specific agg_funs
#' For timepoints with no records, value should show NA
#'
#' @return agg_fun object
#' @noRd
agg_fun_distinct <- function(){
  agg_fun(type = "distinct",
          function_call = quote(
            if(all(is.na(values))){
                NA_integer_
              } else{
                length(unique(values[!is.na(values)]))
              }
            ),
          friendly_name_short = "distinct",
          friendly_name_long = "No. of distinct values"
          )
}

#' midnight_n
#'
#' number of values whose time portion is midnight (used to check for missing time portions)
#' For timepoints with no records, value should show NA
#' @return agg_fun object
#' @noRd
agg_fun_midnight_n <- function(){
  agg_fun(type = "midnight_n",
          function_call = quote(
            if(all(is.na(values))){
                NA_integer_
              } else{
                sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE)
              }
            ),
          friendly_name_short = "midnight_n",
          friendly_name_long = "No. of values with no time element"
          )
}

#' midnight_perc
#'
#' percentage of values whose time portion is midnight (used to check for missing time portions)
#'   out of number of values present
#' For timepoints with no records, value should show NA
#' NOTE: formula returns NaN when all values are NA. Set to NA instead
#' @return agg_fun object
#' @noRd
agg_fun_midnight_perc <- function(){
  agg_fun(type = "midnight_perc",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                100 * sum(format(values, format = "%T") == "00:00:00",
                          na.rm = TRUE
                          ) / length(values[!is.na(values)])
              }
          ),
          friendly_name_short = "midnight_perc",
          friendly_name_long = "Percentage of values with no time element"
          )
}

#' min
#'
#' minimum value, whether numeric or datetime. Excludes NAs
#' NOTE: min/max return Inf and warnings when all values are NA. Set to NA instead
#' Also, min/max drops datetime class if first group is NA so preserve datatypes
#'
#' @param data_class class(es) of the data values
#'
#' @return agg_fun object
#' @noRd
agg_fun_min <- function(data_class = ""){
  if("POSIXct" %in% data_class){
    function_call = quote(
      if(all(is.na(values))){
          as.POSIXct(NA, tz = "UTC")
        } else{
          as.POSIXct(min(values, na.rm = TRUE), tz = "UTC")
        }
      )
  } else{
    function_call = quote(
      if(all(is.na(values))){
          NA_real_
        } else{
          min(values, na.rm = TRUE)
        }
      )
  }

  agg_fun(type = "min",
          function_call = function_call,
          friendly_name_short = "min",
          friendly_name_long = "Minimum value"
          )

}


#' max
#'
#' maximum value, whether numeric or datetime. Excludes NAs
#' NOTE: min/max return Inf and warnings when all values are NA. Set to NA instead
#' Also, min/max drops datetime class if first group is NA so preserve datatypes
#'
#' @param data_class class(es) of the data values
#'
#' @return agg_fun object
#' @noRd
agg_fun_max <- function(data_class = ""){
  if("POSIXct" %in% data_class){
    function_call = quote(
      if(all(is.na(values))){
          as.POSIXct(NA, tz = "UTC")
        } else{
          as.POSIXct(max(values, na.rm = TRUE), tz = "UTC")
        }
      )
  } else{
    function_call = quote(
      if(all(is.na(values))){
          NA_real_
        } else{
          max(values, na.rm = TRUE)
        }
      )
  }

  agg_fun(type = "max",
          function_call = function_call,
          friendly_name_short = "max",
          friendly_name_long = "Maximum value"
          )
}

#' mean
#'
#' mean value. Excludes NAs
#' For timepoints with no records, value should show NA
#' NOTE: mean returns NaN when all values are NA. Set to NA instead
#' @return agg_fun object
#' @noRd
agg_fun_mean <- function(){
  agg_fun(type = "mean",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                mean(values, na.rm = TRUE)
              }
          ),
          friendly_name_short = "mean",
          friendly_name_long = "Mean value"
          )
}

#' median
#'
#' median value. Excludes NAs
#' For timepoints with no records, value should show NA
#' NOTE: need the as.double() because median can return either the original
#' type or a double, and if a mixture of types are
#' returned, data.table doesn't like it
#' @return agg_fun object
#' @noRd
agg_fun_median <- function(){
  agg_fun(type = "median",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                as.double(stats::median(values, na.rm = TRUE))
              }
          ),
          friendly_name_short = "median",
          friendly_name_long = "Median value"
          )
}

#' min_length
#'
#' minimum character length
#' NOTE: min/max return Inf and warnings when all values are NA. Set to NA instead
#' NOTE: need the as.double() because the formula can return either an integer or
#' double depending on what is in the group, and if a mixture of doubles and
#' integers are returned, data.table doesn't like it
#' @return agg_fun object
#' @noRd
agg_fun_min_length <- function(){
  agg_fun(type = "min_length",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                as.double(
                  min(
                    nchar(as.character(values),
                          keepNA = TRUE),
                    na.rm = TRUE
                  )
                )
              }
            ),
          friendly_name_short = "min_length",
          friendly_name_long = "Minimum string length"
          )
}

#' max_length
#'
#' maximum character length
#' NOTE: min/max return Inf and warnings when all values are NA. Set to NA instead
#' NOTE: need the as.double() because the formula can return either an integer or
#' double depending on what is in the group, and if a mixture of doubles and
#' integers are returned, data.table doesn't like it
#' @return agg_fun object
#' @noRd
agg_fun_max_length <- function(){
  agg_fun(type = "max_length",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                as.double(
                  max(
                    nchar(as.character(values),
                          keepNA = TRUE),
                    na.rm = TRUE
                  )
                )
              }
            ),
          friendly_name_short = "max_length",
          friendly_name_long = "Maximum string length"
          )
}

#' mean_length
#'
#' mean character length
#' NOTE: min/max return warnings when all values are NA, so need to suppress them
#' Also, min/max return Inf when all values are NA. Set to NA instead
#' @return agg_fun object
#' @noRd
agg_fun_mean_length <- function(){
  agg_fun(type = "mean_length",
          function_call = quote(
            if(all(is.na(values))){
                NA_real_
              } else{
                mean(
                  nchar(as.character(values),
                        keepNA = TRUE),
                  na.rm = TRUE
                )
              }
            ),
          friendly_name_short = "mean_length",
          friendly_name_long = "Mean string length"
          )
}

#' subcat_n
#'
#' number of times this particular category value appears
#' @return agg_fun object
#' @noRd
agg_fun_subcat_n <- function(){
  agg_fun(type = "subcat_n",
          function_call = quote(
            sum(values == catval, na.rm = TRUE)
            ),
          friendly_name_short = "subcat_n",
          friendly_name_long = "No. of values in the category"
          )
}

#' subcat_perc
#'
#' percentage this particular category value appears out of number of records
#' include all values in denominator, including NA and NaN
#' @return agg_fun object
#' @noRd
agg_fun_subcat_perc <- function(){
  agg_fun(type = "subcat_perc",
          function_call = quote(
            100 * sum(values == catval, na.rm = TRUE) / length(values)
            ),
          friendly_name_short = "subcat_perc",
          friendly_name_long = "Percentage of values in the category"
          )
}


#' Get the correct agg_fun constructor from the string code
#'
#' @param type string code for the aggregation_function
#' @param ... other params to be passed through
#' @return agg_fun object
#' @noRd
agg_fun_from_type <- function(type, ...){
  switch(type,
         "n" = agg_fun_n(),
         "missing_n" = agg_fun_missing_n(),
         "missing_perc" = agg_fun_missing_perc(),
         "nonconformant_n" = agg_fun_nonconformant_n(),
         "nonconformant_perc" = agg_fun_nonconformant_perc(),
         "sum" = agg_fun_sum(),
         "nonzero_perc" = agg_fun_nonzero_perc(),
         "distinct" = agg_fun_distinct(),
         "midnight_n" = agg_fun_midnight_n(),
         "midnight_perc" = agg_fun_midnight_perc(),
         "min" = agg_fun_min(...),
         "max" = agg_fun_max(...),
         "mean" = agg_fun_mean(),
         "median" = agg_fun_median(),
         "min_length" = agg_fun_min_length(),
         "max_length" = agg_fun_max_length(),
         "mean_length" = agg_fun_mean_length(),
         "subcat_n" = agg_fun_subcat_n(),
         "subcat_perc" = agg_fun_subcat_perc(),
         stop(paste("Unrecognised aggregation type:", type),
              call. = FALSE)
         )
}


#' Select the relevant aggregation and append to grouped_values
#'
#' grouped_values is updated byref
#' @param aggregation_function string code for aggregation_function
#' @param data_field_dt this contains all values present in the original data_field, alongside their timepoint_group
#' @param grouped_values this will contain the agg_fun values after aggregating (one column per agg_fun)
#' @param show_progress Print progress to console
#' @noRd
#' @importFrom data.table ':=' .EACHI
aggregate_and_append_values <- function(aggregation_function,
                                        data_field_dt,
                                        grouped_values,
                                        stratify = FALSE,
                                        show_progress = TRUE){
  # initialise known column names to prevent R CMD check notes
  values <- NULL

  agg_fun <- agg_fun_from_type(type = aggregation_function,
                               data_class = class(data_field_dt[["values"]][[1]]))

  if (aggregation_function %in% c("subcat_n", "subcat_perc")) {
    aggregate_and_append_values_subcat(agg_fun, data_field_dt, grouped_values, show_progress)
  } else if (stratify) {
    aggregate_and_append_values_stratified(agg_fun, data_field_dt, grouped_values)
  } else{
    aggregate_and_append_values_simple(agg_fun, data_field_dt, grouped_values)
  }
}

#' Perform the relevant aggregation and append to grouped_values
#'
#' appends a single column
#' grouped_values is updated byref
#' @param agg_fun agg_fun object
#' @param data_field_dt this contains all values present in the original data_field, alongside their timepoint_group
#' @param grouped_values this will contain the agg_fun values after aggregating (one column per subcat)
#' @noRd
aggregate_and_append_values_simple <- function(agg_fun, data_field_dt, grouped_values){
  # initialise known column names to prevent R CMD check notes
  value <- timepoint_group <- NULL

  # aggregate the data_field values before appending to grouped_values in order
  # to distinguish between missing values and missing records
  grouped_values[
    data_field_dt[
      ,
      list("value" = eval(agg_fun$function_call)),
      by = list(timepoint_group)
    ],
    (agg_fun$type) := value,
    by = .EACHI
  ]

  if (!is.na(agg_fun$value_if_no_records)) {
    # update the column just appended
    grouped_values[is.na(get(agg_fun$type)), (agg_fun$type) := agg_fun$value_if_no_records]
  }

}

#' Perform the relevant subcat aggregation and append to grouped_values
#'
#' subcat aggregation functions are different to the others in that they create
#' a separate column per category value
#' grouped_values is updated byref
#' @param agg_fun agg_fun object
#' @param data_field_dt this contains all values present in the original data_field, alongside their timepoint_group
#' @param grouped_values this will contain the agg_fun values after aggregating (one column per subcat)
#' @param show_progress Print progress to console
#' @noRd
aggregate_and_append_values_subcat <- function(agg_fun, data_field_dt, grouped_values, show_progress = TRUE){
  # initialise known column names to prevent R CMD check notes
  value <- values <- timepoint_group <- NULL

  # need to create a separate column per category value
  distinct_categories <-
    sort(data_field_dt[is.na(values) == FALSE, unique(values)])
  log_message(paste0("    ", length(distinct_categories), " categories found"), show_progress)

  # If there is only one category, don't bother
  if (length(distinct_categories) > 1) {
    # TODO: consider setting a max number of categories
    for (j in seq_along(distinct_categories)) {
      log_message(paste0("    ", j, ": ", distinct_categories[j]), show_progress)
      catval <- distinct_categories[j]
      catname <-
        paste0(agg_fun$type, "_", j, "_", gsub("([[:punct:]])|\\s+", "_", catval))
      grouped_values[
        data_field_dt[
          ,
          list("value" = eval(agg_fun$function_call)),
          by = list(timepoint_group)
        ],
        (catname) := value,
        by = .EACHI
      ]
    }
  }
}

#' Perform the relevant stratified aggregation and append to grouped_values
#'
#' appends two columns,
#' grouped_values is updated byref
#' @param agg_fun agg_fun object
#' @param data_field_dt this contains all values present in the original data_field, alongside their timepoint_group and stratify_by_group
#' @param grouped_values this will contain the agg_fun values after aggregating (one column per subcat)
#' @noRd
aggregate_and_append_values_stratified <- function(agg_fun, data_field_dt, grouped_values){
  # initialise known column names to prevent R CMD check notes
  value <- timepoint_group <- stratify_by_group <- NULL

  # aggregate the data_field values before appending to grouped_values in order
  # to distinguish between missing values and missing records
  grouped_values[
    data_field_dt[
      ,
      list("value" = eval(agg_fun$function_call)),
      by = list(timepoint_group, stratify_by_group)
    ],
    (agg_fun$type) := value,
    by = .EACHI
  ]

  if (!is.na(agg_fun$value_if_no_records)) {
    # update the column just appended
    grouped_values[is.na(get(agg_fun$type)), (agg_fun$type) := agg_fun$value_if_no_records]
  }

}

