# daiquiri (development version)

There are a lot of breaking changes as objects have been renamed for better consistency and style

## Breaking changes

* `field_types()` replaces `fieldtypes()`.

* `create_report()`, `prepare_data()`, `aggregate_data()`, and `report_data()` parameters have been renamed.

* `initialise_log()` replaces `log_initialise()`.

* `close_log()` replaces `log_close()`.

* `print_field_types_template()` replaces `field_types_template()` 

## Bug fixes and minor improvements

* Fixed error when user passes in a data.table (to `create_report()` or `prepare_data()`) that contains non-character columns.

* `create_report()` and `report_data()` accept a new parameter `report_title`.

* `close_log()` now returns the path to the closed log file (if any).

* `example_prescriptions` replaces `example_dataset` as the example dataset supplied with the package.

# daiquiri 0.7.0 (2022-04-20)

This release moves the reading of csv files out into a separate function in order to make it more configurable and to handle the parsing of all fields as character data for the user.

## Breaking changes

* `create_report()` now only accepts a dataframe as the first parameter. The `textfile_contains_column_names` parameter has been removed.

* `load_data()` has been replaced with `read_data()` and `prepare_data()`.

* `log_initialise()` function: `dirpath` parameter renamed to `log_directory`.

## New features

* New function `read_data()` reads data from a delimited file, with all columns read in as character type.

* New function `prepare_data()` validates a dataframe against a field_types specification, and prepares it for aggregation.

* `create_report()` accepts a new parameter `dataset_description` for the user to specify a dataset description to appear on the report.

* `export_aggregated_data()` function accepts new `save_file_prefix` parameter.

* New function `field_types_template()` generates template code for creating a field_types specification based on an existing dataframe, and outputs it to the console.

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
