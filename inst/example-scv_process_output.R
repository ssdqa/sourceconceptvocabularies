
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

scv_process_example

#' Execute `scv_output` function
scv_output_example <- scv_output(process_output = scv_process_example,
                                 code_type = 'cdm',
                                 vocab_tbl = NULL) %>%
  suppressMessages()

scv_output_example[[1]]

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(scv_output_example[[1]])
