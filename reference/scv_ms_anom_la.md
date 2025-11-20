# **Multi-Site, Anomaly, Longitudinal**

**Multi-Site, Anomaly, Longitudinal**

## Usage

``` r
scv_ms_anom_la(
  process_output,
  code_type,
  filter_concept,
  filter_mapped,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output from `evp_process`

- code_type:

  type of code to be used in analysis – either `source` or `cdm`; should
  match the code_type provided when running `scv_process`

- filter_concept:

  the code_type concept of interest to be displayed in the output

- filter_mapped:

  the mapped concept of interest to be displayed in the output

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

three graphs:

1.  line graph that shows the smoothed proportion of a concept pair
    across time computation with the Euclidean distance associated with
    each line

2.  line graph that shows the raw proportion of a concept pair across
    time computation with the Euclidean distance associated with each
    line

3.  a bar graph with the Euclidean distance value for each site, with
    the average proportion as the fill

THIS GRAPH SHOWS ONLY ONE CONCEPT PAIR AT A TIME!
