# *Single Site, Exploratory, Cross-Sectional*

*Single Site, Exploratory, Cross-Sectional*

## Usage

``` r
scv_ss_exp_cs(
  process_output,
  code_type,
  facet = NULL,
  num_codes = 10,
  num_mappings = 10
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

- num_mappings:

  the number of top mappings that should be displayed for each code

## Value

a heatmap with one facet per code and additional facet groupings,
limited to the number of codes selected with num_codes mapped code along
the y-axis and proportion of representation as the fill

a reference table with additional information about the codes used as
the facet. includes the code and denominator count, and will also
include the concept name if @vocab_tbl is not NULL
