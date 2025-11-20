# *Single Site, Anomaly, Cross-Sectional*

*Single Site, Anomaly, Cross-Sectional*

## Usage

``` r
scv_ss_anom_cs(
  process_output,
  code_type,
  facet = NULL,
  filter_concept = NULL,
  num_codes = 10,
  num_mappings = 15
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

- filter_concept:

  For instances when the jaccard_index analysis was executed, the
  code_type concept_id of interest for which to display its mappings

- num_codes:

  the number of top codes of code_type that should be displayed in the
  graph

- num_mappings:

  the number of top mappings that should be displayed for each code

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of usage for a
cdm/source concept pair, and the size of the dot represents the mean
proportion for the code type along the x axis
