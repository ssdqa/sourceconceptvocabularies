# Compute Jaccard at the visit level

This function will compute the Jaccard similarity index at the visit
level to determine how often two mappings occur within the same visit
when mapped to/from a given concept

## Usage

``` r
compute_jaccard_scv(
  cohort,
  domain_tbl,
  concept_set,
  code_type,
  code_domain,
  omop_or_pcornet
)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, a patient identifier,
  `start_date`, and `end_date`

- domain_tbl:

  a table with a list of domains and associated metadata; should include
  the name of the domain, the name of the column where source codes can
  be found, the name of the column where CDM codes can be found, and the
  date column that should be used during over time computations

- concept_set:

  a csv file with the source or cdm codes of interest for the analysis.
  should contain at least a `concept_id` column=

- code_type:

  the type of code to be examined in the check; either `source` or `cdm`

- code_domain:

  the domain related to the codes in the `concept_set`; should match a
  domain and its associated metadata in the `domain_tbl` file

- omop_or_pcornet:

  Option to run the function using the OMOP or PCORnet CDM as the
  default CDM

## Value

a dataframe summarizing, for each code in the concept set, the jaccard
similarity index of two mapped concepts within the same visit
