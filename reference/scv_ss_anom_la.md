# *Single Site, Anomaly, Longitudinal*

*Single Site, Anomaly, Longitudinal*

## Usage

``` r
scv_ss_anom_la(process_output, code_type, filter_concept, facet = NULL)
```

## Arguments

- process_output:

  dataframe output by `scv_process`

- code_type:

  type of code to be used in analysis – either `source` or `cdm`; should
  match the code_type provided when running `scv_process`

- filter_concept:

  the code_type concept of interest to be displayed in the output

- facet:

  the variables by which you would like to facet the graph

## Value

if analysis was executed by year or greater, a C control chart showing
the number of mappings for filter_concept is returned with outliers
marked with orange dots

        if analysis was executed by month or smaller, an STL regression is
        conducted and outliers are marked with red dots. the graphs representing
        the data removed in the regression are also returned
