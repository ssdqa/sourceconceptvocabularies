# SCV Sample Domain File

A sample version of the file structure expected for the `domain_tbl`
parameter in the `scv_process` function. The user should recreate this
file and include their own domain definitions.

## Usage

``` r
scv_domain_file
```

## Format

### `scv_domain_file`

A data frame with 5 columns

- domain:

  The name of the CDM table associated with the domain where the concept
  of interest can be found. Should match the domain listed in the
  concept_set file.

- concept_field:

  The name of the column in the domain table that contains the concepts
  of interest

- source_concept_field:

  The name of the column in the domain table that contains the source
  concepts of interest

- date_field:

  The name of the column in the domain table that contains dates to be
  used for time-based filtering.

- vocabulary_field:

  (PCORnet only) The name of the column in the domain table where the
  vocabulary type is stored
