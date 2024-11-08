## code to prepare `scv_domain_file` dataset goes here

scv_domain_file <- tidyr::tibble('domain' = c('condition_occurrence', 'diagnosis'),
                                 'concept_field' = c('condition_concept_id', 'dx'),
                                 'source_concept_field' = c('condition_source_concept_id', 'raw_dx'),
                                 'date_field' = c('condition_start_date', 'admit_date'),
                                 'vocabulary_field' = c(NA, 'dx_type'))

usethis::use_data(scv_domain_file, overwrite = TRUE)
