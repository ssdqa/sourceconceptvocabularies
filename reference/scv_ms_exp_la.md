# *Multi Site, Exploratory, Longitudinal*

*Multi Site, Exploratory, Longitudinal*

## Usage

``` r
scv_ms_exp_la(
  process_output,
  code_type,
  filter_concept,
  num_mappings = 10,
  facet = NULL,
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

- filter_concept:

  the code_type concept of interest for which mappings should be shown

- num_mappings:

  an integer indicating the top number of mappings for filter_concept
  that should be displayed

- facet:

  the variables by which you would like to facet the graph

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a line graph with one facet per code displaying the proportion of mapped
codes across the user selected time period

a reference table with total counts of each code across the entire user
selected time period
