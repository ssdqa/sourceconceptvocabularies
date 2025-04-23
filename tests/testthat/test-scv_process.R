
## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(scv_process(cohort = cht,
                           code_type = 'cdm',
                           multi_or_single_site = 'test',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'omop'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(scv_process(cohort = cht,
                           code_type = 'cdm',
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'test',
                           omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(scv_process(cohort = cht,
                           code_type = 'cdm',
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'test'))
})


## Generally checking that code runs
test_that('scv exp nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_no_error(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension'),
                              domain_tbl = scv_file,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              code_domain = 'condition_occurrence'))
})

test_that('scv ss anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_warning(expect_warning(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension'),
                              domain_tbl = scv_file,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              code_domain = 'condition_occurrence')))
})


test_that('scv ss anom nt jaccard -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_no_error(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension'),
                              domain_tbl = scv_file,
                              jaccard_index = TRUE,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              code_domain = 'condition_occurrence'))
})


test_that('scv ms anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_warning(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension'),
                              domain_tbl = scv_file,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              code_domain = 'condition_occurrence'))
})


test_that('scv exp at -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_no_error(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension') %>%
                                filter(concept_id == 132685),
                              domain_tbl = scv_file,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              code_domain = 'condition_occurrence',
                              time = TRUE,
                              time_period = 'year',
                              time_span = c('2018-01-01', '2020-01-01')))

  expect_error(scv_process(cohort = cohort,
                            concept_set = load_codeset('dx_hypertension'),
                            domain_tbl = scv_file,
                            omop_or_pcornet = 'omop',
                            code_type = 'cdm',
                            multi_or_single_site = 'single',
                            anomaly_or_exploratory = 'exploratory',
                            code_domain = 'condition_occurrence',
                            time = TRUE,
                            time_period = 'year',
                            time_span = c('2018-01-01', '2020-01-01')))
})

test_that('scv ss anom at -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_error(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension') %>%
                                filter(concept_id == 132685),
                              domain_tbl = scv_file,
                              code_type = 'cdm',
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              code_domain = 'condition_occurrence',
                              time = TRUE,
                              time_period = 'year',
                              time_span = c('2018-01-01', '2020-01-01')))
})


test_that('scv ms anom at -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'scv_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  scv_file <- tibble(domain = c('condition_occurrence'),
                     concept_field = c('condition_concept_id'),
                     date_field = c('condition_start_date'),
                     source_concept_field = c('condition_source_concept_id'),
                     vocabulary_field = c(NA))

  expect_error(scv_process(cohort = cohort,
                              concept_set = load_codeset('dx_hypertension') %>%
                                filter(concept_id == 132685),
                              code_type = 'cdm',
                              domain_tbl = scv_file,
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              code_domain = 'condition_occurrence',
                              time = TRUE,
                              time_period = 'year',
                              time_span = c('2018-01-01', '2020-01-01')))
})

# test_that('testing pcornet version', {
#
#   rlang::is_installed("DBI")
#   rlang::is_installed("readr")
#   rlang::is_installed('RSQLite')
#
#   conn <- mk_testdb_omop()
#
#   initialize_dq_session(session_name = 'prc_process_test',
#                         working_directory = getwd(),
#                         db_conn = conn,
#                         is_json = FALSE,
#                         file_subdirectory = 'testspecs',
#                         cdm_schema = NA)
#
#   cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
#     mutate(start_date = as.Date('2018-01-01'),
#            end_date = as.Date('2020-01-01'),
#            site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))
#
#   scv_file <- tibble(domain = c('condition_occurrence'),
#                      concept_field = c('condition_concept_id'),
#                      date_field = c('condition_start_date'),
#                      source_concept_field = c('condition_source_concept_id'),
#                      vocabulary_field = c(NA))
#
#   expect_no_error(scv_process(cohort = cohort,
#                               concept_set = load_codeset('dx_hypertension'),
#                               domain_tbl = scv_file,
#                               omop_or_pcornet = 'pcornet',
#                               multi_or_single_site = 'single',
#                               anomaly_or_exploratory = 'exploratory',
#                               code_domain = 'condition_occurrence'))
# })
