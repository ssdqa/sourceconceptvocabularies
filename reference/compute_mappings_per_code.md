# Compute number and median mappings per code

Compute number and median mappings per code

## Usage

``` r
compute_mappings_per_code(tbl, col, denom, facet = NULL)
```

## Arguments

- tbl:

  intermediate table generated in the output function that contains the
  concepts of interest to be used to compute number of mappings

- col:

  the name of the column with the concept that needs to be summarized

- denom:

  the denominator count associated with @col

- facet:

  grouping variables to be used to compute group-specific counts and
  distance from overall median

## Value

dataframe that summarizes the overall median number of mappings,
group-specific number of mappings, and how many MAD from the overall
median a code falls
