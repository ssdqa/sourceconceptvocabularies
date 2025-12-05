# Source and Concept Vocabularies

This is a concept-set testing module that will compute frequency
distributions for the usage of either source-to-concept or
concept-to-source concept pairs in order to highlight mapping patterns
and impacts of concept standardization. The user will provide the domain
definitions (`domain_tbl`) and a concept set with the concepts of
interest (`concept_set`). Sample versions of these inputs are included
as data in the package and are accessible with
`sourceconceptvocabularies::`. Results can optionally be stratified by
site, age group, and/or time. This function is compatible with both the
OMOP and the PCORnet CDMs based on the user's selection.

## Usage

``` r
scv_process(
  cohort,
  concept_set,
  omop_or_pcornet,
  domain_tbl = sourceconceptvocabularies::scv_domain_file,
  code_type,
  code_domain,
  jaccard_index = FALSE,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  p_value = 0.9,
  age_groups = NULL,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- concept_set:

  *tabular input or vector* \|\| **required**

  For analyses where `time = FALSE`, this input should be a table
  containing the standard CDM concepts **OR** source concepts of
  interest for the analysis. This input should contain at least one of
  following:

  - `concept_id` \| *integer* \| the concept_id of interest (required
    for OMOP)

  - `concept_code` \| *character* \| the code of interest (required for
    PCORnet)

  For certain PCORnet applications, it should also contain

  - `vocabulary_id` \| *character* \| the vocabulary of the code, which
    should match what is listed in the domain table's `vocabulary_field`

  For analyses where `time = TRUE`, this input should be a vector with
  up to 5 standard CDM **OR** source concepts of interest for the
  analysis. This limitation is applied to reduce computational strain.
  We recommend running a cross-sectional analysis first to identify
  potential concepts of interest, then using these as input for the
  longitudinal analysis.

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- domain_tbl:

  *tabular input* \|\| **required**

  A table that defines the domains where concepts should be identified.
  This input should contain:

  - `domain` \| *character* \| the name of the CDM table where the
    concepts can be identified

  - `concept_field` \| *character* \| the name of the field in the CDM
    table where standard `cdm` codes can be identified (i.e.
    drug_concept_id or dx)

  - `source_concept_field` \| *character* \| the name of the field in
    the CDM table where `source` codes can be identified (i.e.
    drug_source_concept_id or raw_dx)

  - `date_field` \| *character* \| the name of the field in the CDM
    table that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  To see an example of this input, see
  [`?sourceconceptvocabularies::scv_domain_file`](https://ssdqa.github.io/sourceconceptvocabularies/reference/scv_domain_file.md)

- code_type:

  *string* \|\| **required**

  A string identifying the type of concept that has been provided in the
  `concept_set`.

  Acceptable values are `cdm` (the standard, mapped code that is
  included in the CDM) or `source` (the "raw" concept from the source
  system)

- code_domain:

  *string* \|\| **required**

  The string name of the domain where the concepts can be identified.
  This input should match at least one of the domains in the
  `domain_tbl`, and it will function to filter this table down to only
  the relevant domain and allow the user to store multiple domains in
  this table for reuse in other analyses.

- jaccard_index:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether a Jaccard index should be computed at the
  visit level to determine how often two mapped concepts cooccur in the
  same encounter. This computation can help identify potential instances
  of post-coordination for SNOMED concepts.

  This is only applicable for the
  `Single Site, Anomaly Detection, Cross-Sectional` check.

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \| Option to conduct an exploratory or anomaly detection
  analysis. Exploratory analyses give a high level summary of the data
  to examine the fact representation within the cohort. Anomaly
  detection analyses are specialized to identify outliers within the
  cohort.

- p_value:

  *numeric* \|\| defaults to `0.9`

  The p value to be used as a threshold in the Multi-Site, Anomaly
  Detection, Cross-Sectional analysis

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

## Value

This function will return a dataframe summarizing the mapping patterns
for each concept provided by the user. For a more detailed description
of output specific to each check type, see the PEDSpace metadata
repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'sourceconceptvocabularies'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'scv_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
scv_domain_tbl <- dplyr::tibble(domain = 'condition_occurrence',
                                concept_field = 'condition_concept_id',
                                source_concept_field =
                                  'condition_source_concept_id',
                                date_field = 'condition_start_date',
                                vocabulary_field = NA)

scv_concept_set <- read_codeset('dx_hypertension')

#' Execute `scv_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
scv_process_example <- scv_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   code_type = 'cdm',
                                   code_domain = 'condition_occurrence',
                                   domain_tbl = scv_domain_tbl,
                                   concept_set = scv_concept_set) %>%
  suppressMessages()
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `scv_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output, code_type                    │
#> │ Required for Check: num_codes, num_mappings                   │
#> │ Optional: vocab_tbl                                           │
#> │                                                               │
#> │ See ?scv_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

scv_process_example
#> # A tibble: 1 × 10
#>   site     domain            concept_id source_concept_id    ct denom_concept_ct
#>   <chr>    <chr>                  <int>             <int> <int>            <int>
#> 1 combined condition_occurr…     320128            320128     5                5
#> # ℹ 4 more variables: denom_source_ct <int>, concept_prop <dbl>,
#> #   source_prop <dbl>, output_function <chr>

#' Execute `scv_output` function
scv_output_example <- scv_output(process_output = scv_process_example,
                                 code_type = 'cdm',
                                 vocab_tbl = NULL) %>%
  suppressMessages()

scv_output_example[[1]]


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(scv_output_example[[1]])

{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='ggiraph-svg' role='graphics-document' id='svg_8f478bf74146d119' viewBox='0 0 432 360'>\n <defs id='svg_8f478bf74146d119_defs'>\n  <clipPath id='svg_8f478bf74146d119_c1'>\n   <rect x='0' y='0' width='432' height='360'/>\n  <\/clipPath>\n  <clipPath id='svg_8f478bf74146d119_c2'>\n   <rect x='49.01' y='44.36' width='294.78' height='298.99'/>\n  <\/clipPath>\n  <clipPath id='svg_8f478bf74146d119_c3'>\n   <rect x='49.01' y='21.08' width='294.78' height='23.28'/>\n  <\/clipPath>\n <\/defs>\n <g id='svg_8f478bf74146d119_rootg' class='ggiraph-svg-rootg'>\n  <g clip-path='url(#svg_8f478bf74146d119_c1)'>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round' class='ggiraph-svg-bg'/>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='none'/>\n  <\/g>\n  <g clip-path='url(#svg_8f478bf74146d119_c2)'>\n   <polyline points='49.01,193.86 343.79,193.86' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='196.40,343.35 196.40,44.36' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <rect id='svg_8f478bf74146d119_e1' x='73.58' y='69.28' width='245.65' height='249.15' fill='#FBB761' fill-opacity='1' stroke='none' title='Mapped Concept Name: No vocabulary table input'/>\n   <text x='193.69' y='196.97' font-size='6.4pt' font-family='DejaVu Sans'>1<\/text>\n  <\/g>\n  <g clip-path='url(#svg_8f478bf74146d119_c3)'>\n   <text x='182.69' y='31.46' font-size='5.4pt' font-family='DejaVu Sans' fill='#1A1A1A' fill-opacity='1'>320128<\/text>\n   <text x='163.79' y='39.23' font-size='5.4pt' font-family='DejaVu Sans' fill='#1A1A1A' fill-opacity='1'> Total Mappings: 1<\/text>\n  <\/g>\n  <g clip-path='url(#svg_8f478bf74146d119_c1)'>\n   <text x='16.65' y='196.48' font-size='5.4pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>320128<\/text>\n   <text x='172.11' y='352.65' font-size='6.75pt' font-family='DejaVu Sans'>concept_id<\/text>\n   <text transform='translate(12.04,235.34) rotate(-90.00)' font-size='6.75pt' font-family='DejaVu Sans'>source_concept_id<\/text>\n   <text x='360.23' y='151.2' font-size='6.75pt' font-family='DejaVu Sans'>concept_prop<\/text>\n   <image x='360.23' y='157.61' width='17.28' height='86.4' preserveAspectRatio='none' xlink:href='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAEsCAYAAAACUNnVAAAAHUlEQVQ4jWP4vT3xPxMDAwPDKDFKjBKjxCgxXAgA1SgFadr2HN0AAAAASUVORK5CYII=' xmlns:xlink='http://www.w3.org/1999/xlink'/>\n   <polyline points='374.05,200.81 377.51,200.81' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.37' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='363.69,200.81 360.23,200.81' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.37' stroke-linejoin='round' stroke-linecap='butt'/>\n   <text x='382.99' y='203.43' font-size='5.4pt' font-family='DejaVu Sans'>1<\/text>\n   <text x='49.01' y='13.36' font-size='8.1pt' font-family='DejaVu Sans'>Top 25 Mappings for Top 10 CDM Codes<\/text>\n  <\/g>\n <\/g>\n<\/svg>","js":null,"uid":"svg_8f478bf74146d119","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}","placement":"doc","opacity":0.9,"offx":10,"offy":10,"use_cursor_pos":true,"use_fill":false,"use_stroke":false,"delay_over":200,"delay_out":500},"hover":{"css":".hover_data_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_data_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_data_SVGID_ { fill:orange;stroke:black; }\nline.hover_data_SVGID_, polyline.hover_data_SVGID_ { fill:none;stroke:orange; }\nrect.hover_data_SVGID_, polygon.hover_data_SVGID_, path.hover_data_SVGID_ { fill:orange;stroke:none; }\nimage.hover_data_SVGID_ { stroke:orange; }","reactive":true,"nearest_distance":null},"hover_inv":{"css":""},"hover_key":{"css":".hover_key_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_key_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_key_SVGID_ { fill:orange;stroke:black; }\nline.hover_key_SVGID_, polyline.hover_key_SVGID_ { fill:none;stroke:orange; }\nrect.hover_key_SVGID_, polygon.hover_key_SVGID_, path.hover_key_SVGID_ { fill:orange;stroke:none; }\nimage.hover_key_SVGID_ { stroke:orange; }","reactive":true},"hover_theme":{"css":".hover_theme_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_theme_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_theme_SVGID_ { fill:orange;stroke:black; }\nline.hover_theme_SVGID_, polyline.hover_theme_SVGID_ { fill:none;stroke:orange; }\nrect.hover_theme_SVGID_, polygon.hover_theme_SVGID_, path.hover_theme_SVGID_ { fill:orange;stroke:none; }\nimage.hover_theme_SVGID_ { stroke:orange; }","reactive":true},"select":{"css":".select_data_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_data_SVGID_ { stroke:none;fill:red; }\ncircle.select_data_SVGID_ { fill:red;stroke:black; }\nline.select_data_SVGID_, polyline.select_data_SVGID_ { fill:none;stroke:red; }\nrect.select_data_SVGID_, polygon.select_data_SVGID_, path.select_data_SVGID_ { fill:red;stroke:none; }\nimage.select_data_SVGID_ { stroke:red; }","type":"multiple","only_shiny":true,"selected":[]},"select_inv":{"css":""},"select_key":{"css":".select_key_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_key_SVGID_ { stroke:none;fill:red; }\ncircle.select_key_SVGID_ { fill:red;stroke:black; }\nline.select_key_SVGID_, polyline.select_key_SVGID_ { fill:none;stroke:red; }\nrect.select_key_SVGID_, polygon.select_key_SVGID_, path.select_key_SVGID_ { fill:red;stroke:none; }\nimage.select_key_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"select_theme":{"css":".select_theme_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_theme_SVGID_ { stroke:none;fill:red; }\ncircle.select_theme_SVGID_ { fill:red;stroke:black; }\nline.select_theme_SVGID_, polyline.select_theme_SVGID_ { fill:none;stroke:red; }\nrect.select_theme_SVGID_, polygon.select_theme_SVGID_, path.select_theme_SVGID_ { fill:red;stroke:none; }\nimage.select_theme_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"zoom":{"min":1,"max":1,"duration":300,"default_on":false},"toolbar":{"position":"topright","pngname":"diagram","tooltips":null,"fixed":false,"hidden":[],"delay_over":200,"delay_out":500},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}
```
