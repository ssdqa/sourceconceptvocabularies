# *Multi-Site, Anomaly, Cross-Sectional*

*Multi-Site, Anomaly, Cross-Sectional*

## Usage

``` r
scv_ms_anom_cs(
  process_output,
  code_type,
  facet = NULL,
  filter_concept,
  num_mappings = 20,
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

- filter_concept:

  the code_type concept_id of interest for which to display its mappings

- num_mappings:

  an integer indicating the number of top mappings that should be
  displayed

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of
representation of a filter_concept/mapping concept pair, and the size of
the dot represents the mean proportion across all sites
