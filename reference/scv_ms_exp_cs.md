# *Multi Site, Exploratory, Cross-Sectional*

*Multi Site, Exploratory, Cross-Sectional*

## Usage

``` r
scv_ms_exp_cs(
  process_output,
  code_type,
  facet = NULL,
  num_codes = 10,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  dataframe output by `scv_process`

- code_type:

  type of code to be used in analysis – either `source` or `cdm`; should
  match the code_type provided when running `scv_process`

- facet:

  the variables by which you would like to facet the graph

- num_codes:

  the number of top codes of code_type that should be displayed in the
  graph

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a searchable and filterable table with mappings, proportion of
representation, and denominator counts for the number of codes selected
in num_codes concept name will be included if vocab_tbl is not NULL
