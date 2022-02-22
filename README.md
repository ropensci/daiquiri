
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daiquiri

The daiquiri package generates data quality reports that enable quick
visual review of temporal shifts in record-level data. Time series plots
showing aggregated values are automatically created for each data field
(column) depending on its contents (e.g. min/max/mean values for numeric
data, no. of distinct values for categorical data), as well as overviews
for missing values, non-conformant values, and duplicated rows. The
resulting reports are sharable and can contribute to forming a
transparent record of the entire analysis process. It is designed with
Electronic Health Records in mind, but can be used for any type of
record-level temporal data (i.e. tabular data where each row represents
a single “event”, one column contains the “event date”, and other
columns contain any associated values for the event).

## Why should I use it?

Large routinely-collected datasets are increasingly being used in
research. However, given their data are collected for operational rather
than research purposes, there is a greater-than-usual need for them to
be checked for data quality issues before any analyses are conducted.
Events occurring at the institutional level such as software updates,
new machinery or processes can cause temporal artefacts that, if not
identified and taken into account, can lead to biased results and
incorrect conclusions. For example, the figures below show real data
from a large hospital in the UK.

<img src="man/figures/antibiotics_day_DurationEnteredByPrescriber_missing_perc.png" width="350" /><img src="man/figures/bchem_creatinine_day_Value_mean.png" width="350" />

The first figure shows the percentage of missing values in the
‘Duration’ field of a dataset containing antibiotic prescriptions, and
the second figure shows the mean value of all laboratory tests checking
for levels of ‘creatinine’ in the blood. As you can see, these values
can sometimes change suddenly and unnaturally, and researchers need to
take this into account if comparing or combining the data before and
after these ‘change points’.

While these checks should theoretically be conducted by the researcher
at the initial data analysis stage, in practice it is unclear to what
extent this is actually done, since it is rarely, if ever, reported in
published papers. With the increasing drive towards greater transparency
and reproducibility within the scientific community, this essential yet
often-overlooked part of the analysis process will inevitably begin to
come under greater scrutiny. The daiquiri package helps researchers
conduct this part of the process more thoroughly, consistently and
transparently, hence increasing the quality of their studies as well as
trust in the scientific process.

## Getting started

The intention is to make daiquiri available in CRAN but until then, you
can install the latest version from github by doing the following:

1.  Navigate to <https://github.com/phuongquan/daiquiri/releases>

2.  Underneath the most recent release, click to open the Assets
    section, then download the `daiquiri_x.y.z.tar.gz` file (where x.y.z
    is the release number)

3.  To install, run the following, replacing the `file_name_and_path`
    with the path to the downloaded file. NOTE: Depending on what
    packages you already have installed, it may return an error saying
    that certain dependencies are not available for the package. It will
    not install these automatically. Once you have installed the
    dependencies from CRAN, try to install the package again.

``` r
install.packages(file_name_and_path, repos = NULL, type="source")
```

Once installed, the walkthrough vignette guides you through how to use
the package:

``` r
vignette("walkthrough", package = "daiquiri")
```

## Acknowledgements

This work was supported by the National Institute for Health Research
Health Protection Research Unit (NIHR HPRU) in Healthcare Associated
Infections and Antimicrobial Resistance at the University of Oxford in
partnership with Public Health England (PHE) (NIHR200915), and by the
NIHR Oxford Biomedical Research Centre.
