# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

After following the instructions to reproduce the analysis, you will
also need to change the `output_function` column to tell the
`csd_output` function which check you executed. Reference the table
below for the labels that are associated with each check:

| Check Type                                     | output_function |
|:-----------------------------------------------|:----------------|
| Multi Site, Exploratory, Cross-Sectional       | scv_ms_exp_cs   |
| Multi Site, Exploratory, Longitudinal          | scv_ms_exp_la   |
| Multi Site, Anomaly Detection, Cross-Sectional | scv_ms_anom_cs  |
| Multi Site, Anomaly Detection, Longitudinal    | scv_ms_anom_la  |

## Multi-Site Exploratory Analysis

The process for the exploratory analysis is the same for both the
cross-sectional and longitudinal configurations.

First, execute either of the **Single Site, Exploratory** analyses,
configured appropriately for your study, against each data source.

``` r
library(sourceconceptvocabularies)

my_table <- scv_process(cohort = cohort,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        time = T / F,
                        ...)
```

Then, combine these results into a single table with the different sites
delineated in the `site` column.

``` r
my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = '{see table above}')
```

## Multi-Site Anomaly Detection Analysis

For anomaly detection analysis, start by executing the same steps as the
exploratory analysis. Then, you will execute the relevant anomaly
detection algorithm against the resulting table. See below for the
different processes for cross-sectional and longitudinal analysis.

### Cross-Sectional

For a cross-sectional analysis, the `compute_dist_anomalies` and
`detect_outliers` functions, both available through the `squba.gen`
package, should be executed against your results. Copy the code below,
inputting the table you generated.

If standard CDM codes were provided (`code_type = 'cdm'`): - `prop_col`
= `concept_prop` - `var_col` = `concept_id` - `denom_col` =
`denom_concept_ct`

If source codes were provided (`code_type = 'source'`): - `prop_col` =
`source_prop` - `var_col` = `source_concept_id` - `denom_col` =
`denom_source_ct`

The `p_value` can be selected by the user.

``` r
# First execute the compute_dist_anomalies function
df_start <- compute_dist_anomalies(df_tbl = my_table,
                                   grp_vars = c('domain', 'source_concept_id', 'concept_id'),
                                   var_col = prop_col,
                                   denom_cols = c(var_col, denom_col))

# Then, use that output as input for the detect_outliers function
df_final <- detect_outliers(df_tbl = df_start,
                            tail_input = 'both',
                            p_input = p_value,
                            column_analysis = prop_col,
                            column_variable = c('concept_id', 'source_concept_id')) %>%
  dplyr::mutate(output_function = '{see table above}')
```

### Longitudinal

For a longitudinal analysis, the `ms_anom_euclidean` function, available
through the `squba.gen` package, should be executed against your
results. Copy the code below, inputting the data you generated.

If standard CDM codes were provided (`code_type = 'cdm'`), `prop_col`
should be set to `concept_prop`. If source codes were provided
(`code_type = 'source'`), `prop_col` should be set to `source_prop`.

``` r
df <- ms_anom_euclidean(fot_input_tbl = csd_tbl,
                        grp_vars = c('site', 'concept_id', 'source_concept_id'),
                        var_col = prop_col) %>%
  dplyr::mutate(output_function = '{see table above}')
```
