% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_data.R
\name{export_aggregated_data}
\alias{export_aggregated_data}
\title{Export aggregated data}
\usage{
export_aggregated_data(
  aggregated_data,
  save_directory,
  save_file_prefix = "",
  save_file_type = "csv"
)
}
\arguments{
\item{aggregated_data}{A \code{daiquiri_aggregated_data} object}

\item{save_directory}{String. Full or relative path for save folder}

\item{save_file_prefix}{String. Optional prefix for the exported filenames}

\item{save_file_type}{String. Filetype extension supported by \code{readr},
currently only csv allowed}
}
\value{
(invisibly) The \code{daiquiri_aggregated_data} object that was passed in
}
\description{
Export aggregated data to disk.  Creates a separate file for each aggregated
field in dataset.
}
\examples{
\donttest{
raw_data <- read_data(
  system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
  delim = ",",
  col_names = TRUE
)

source_data <- prepare_data(
  raw_data,
  field_types = field_types(
    PrescriptionID = ft_uniqueidentifier(),
    PrescriptionDate = ft_timepoint(),
    AdmissionDate = ft_datetime(includes_time = FALSE),
    Drug = ft_freetext(),
    Dose = ft_numeric(),
    DoseUnit = ft_categorical(),
    PatientID = ft_ignore(),
    Location = ft_categorical(aggregate_by_each_category = TRUE)
  ),
  override_column_names = FALSE,
  na = c("", "NULL")
)

aggregated_data <- aggregate_data(
  source_data,
  aggregation_timeunit = "day"
)

export_aggregated_data(
  aggregated_data,
  save_directory = ".",
  save_file_prefix = "ex_"
)

\dontshow{
f <- list.files(".", "^ex_.*csv$")
file.remove(f)
}
}

}
