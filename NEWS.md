# daiquiri 1.0.3 (2022-12-06)

## Bug fixes and minor improvements

* Validation warnings now match column names correctly when `field_types` are specified in a different order to the supplied data frame columns

* Passing in a data frame containing integer columns no longer causes an aggregation error (#9)

* Calling functions with package prefix no longer causes an error (#10)

* Fixed (some) errors about duplicate chunk labels when running package from within rmarkdown/quarto (rmd/qmd) files (bug introduced in previous release 1.0.2). This now allows chunks in the parent file to be unlabelled but unfortunately still errors when there is a chunk labelled `setup`. (#7)


# daiquiri 1.0.2 (2022-11-21)

* When rendering reports, intermediate files are now written to `tempdir()` instead of to the directory of the `report_htmldoc.Rmd` file (the default behaviour of `rmarkdown::render()`). This fixes errors caused when the library location is read-only.


# daiquiri 1.0.1 (2022-11-11)

First release to CRAN

* Replaced calls to deprecated function `aes_string()` in `ggplot2`


# daiquiri 1.0.0 (2022-11-01)

This release incorporates changes requested for acceptance into https://ropensci.org/. There are many breaking changes as objects have been renamed for better consistency and style.

## Breaking changes

* `daiquiri_report()` replaces `create_report()` and some parameters have been renamed.

* `field_types()` replaces `fieldtypes()`.

* `prepare_data()`, `aggregate_data()`, and `report_data()` parameters have been renamed.

* `initialise_log()` replaces `log_initialise()`.

* `close_log()` replaces `log_close()`.

* `template_field_types()` replaces `fieldtypes_template()` 

## Bug fixes and minor improvements

* Fixed error when user passes in a `data.table` (to `daiquiri_report()` or `prepare_data()`) that contains non-character columns.

* `daiquiri_report` (formerly `create_report()`) and `report_data()` accept a new parameter `report_title`.

* `report_data()` now accepts `...` parameter to be passed through to `rmarkdown::render()`.

* `close_log()` now returns the path to the closed log file (if any).

* `example_prescriptions.csv` replaces `example_dataset.csv` as the example dataset supplied with the package.

# daiquiri 0.7.0 (2022-04-20)

This release moves the reading of csv files out into a separate function in order to make it more configurable and to handle the parsing of all fields as character data for the user.

## Breaking changes

* `create_report()` now only accepts a dataframe as the first parameter. The `textfile_contains_columnnames` parameter has been removed.

* `load_data()` has been replaced with `read_data()` and `prepare_data()`.

* `log_initialise()` function: `dirpath` parameter renamed to `log_directory`.

## New features

* New function `read_data()` reads data from a delimited file, with all columns read in as character type.

* New function `prepare_data()` validates a dataframe against a fieldtypes specification, and prepares it for aggregation.

* `create_report()` accepts a new parameter `dataset_shortdesc` for the user to specify a dataset description to appear on the report.

* `export_aggregated_data()` function accepts new `save_fileprefix` parameter.

* New function `fieldtypes_template()` generates template code for creating a fieldtypes specification based on an existing dataframe, and outputs it to the console.

## Bug fixes and minor improvements

* Fixed ALL_FIELDS_COMBINED calculated field rowsumming NAs incorrectly.

* Fixed plots failing when all values are missing.

* Fixed `log_message()` trying to write to different log file when called from Rmd folder (and relative path used).

* Made '[DUPLICATES]' and '[ALL_FIELDS_COMBINED]' reserved names for data fields.

* Allow column names in supplied dataframe to contain special characters.

* Reduced real estate at top of report.

* Removed datatype column and fixed validation warnings total from Source data tab in report.

* Updated example data.

* Added further validation checks for user-supplied params.

* Added CITATION file.


# daiquiri 0.6.1 (2022-02-23)

Beta release. Complete list of functions exported:

* `aggregate_data()`
* `create_report()` accepts either a dataframe or csv filename as the first parameter. This may change in future.
* `export_aggregated_data()`
* `field_types()`
* `ft_categorical()`
* `ft_datetime()`
* `ft_freetext()`
* `ft_ignore()`
* `ft_numeric()`
* `ft_simple()`
* `ft_timepoint()`
* `ft_uniqueidentifier()`
* `load_data()` accepts either a dataframe or csv filename as the first parameter. This may change in future.
* `log_close()`
* `log_initialise()`
* `report_data()`
