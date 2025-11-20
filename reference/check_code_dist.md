# Base SCV function

Base SCV function

## Usage

``` r
check_code_dist(
  cohort,
  concept_set,
  code_type,
  code_domain,
  time = FALSE,
  omop_or_pcornet,
  domain_tbl = sourceconceptvocabularies::scv_domain_file
)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, `person_id`,
  `start_date`, and `end_date`

- concept_set:

  for analyses where time = FALSE, a csv file with the source or cdm
  codes of interest for the analysis. should contain at least a
  `concept_id` column

                     for analyses where time = TRUE, a vector with up to 5 source or cdm codes of interest for the analysis.

- code_type:

  the type of code to be examined in the check; either `source` or `cdm`

- code_domain:

  the domain related to the codes in the `concept_set`; should match a
  domain and its associated metadata in the `domain_tbl` file

- time:

  logical to indicate whether the user would like to examine mappings
  over time or not

- omop_or_pcornet:

  Option to run the function using the OMOP or PCORnet CDM as the
  default CDM

- domain_tbl:

  a table with a list of domains and associated metadata; should include
  the name of the domain, the name of the column where source codes can
  be found, the name of the column where CDM codes can be found, and the
  date column that should be used during over time computations

## Value

one dataframe with counts and proportions of source -\> cdm or cdm -\>
source mapping pairs for each facet group. if time = FALSE, these counts
are computed overall. if time = TRUE, these counts are computed for each
user-specified time increment.
