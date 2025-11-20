# *Single Site, Exploratory, Longitudinal*

*Single Site, Exploratory, Longitudinal*

## Usage

``` r
scv_ss_exp_la(process_output, code_type, num_mappings = 10, facet = NULL)
```

## Arguments

- process_output:

  dataframe output by `scv_process`

- code_type:

  type of code to be used in analysis – either `source` or `cdm`; should
  match the code_type provided when running `scv_process`

- num_mappings:

  integer indicating the number of top mappings per code to be displayed
  in the output

- facet:

  the variables by which you would like to facet the graph

## Value

a line graph with one facet per code displaying the proportion of mapped
codes across the user selected time period

a reference table with total counts of each code across the entire user
selected time period
