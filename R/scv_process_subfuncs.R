

#' Base SCV function
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param concept_set for analyses where time = FALSE, a csv file with the source or cdm codes of interest for the analysis.
#'                    should contain at least a `concept_id` column
#'
#'                    for analyses where time = TRUE, a vector with up to 5 source or cdm codes of interest for the analysis.
#' @param code_type the type of code to be examined in the check; either `source` or `cdm`
#' @param code_domain the domain related to the codes in the `concept_set`; should match a domain and
#'                    its associated metadata in the `domain_tbl` file
#' @param time logical to indicate whether the user would like to examine mappings over time or not
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' @param domain_tbl a table with a list of domains and associated metadata; should include the name
#'                   of the domain, the name of the column where source codes can be found, the name
#'                   of the column where CDM codes can be found, and the date column that should
#'                   be used during over time computations
#'
#' @return one dataframe with counts and proportions of source -> cdm or cdm -> source mapping pairs
#'         for each facet group. if time = FALSE, these counts are computed overall. if time = TRUE,
#'         these counts are computed for each user-specified time increment.
#'
#' @importFrom purrr set_names
#'
check_code_dist <- function(cohort,
                            concept_set,
                            code_type,
                            code_domain,
                            time = FALSE,
                            omop_or_pcornet,
                            domain_tbl = sourceconceptvocabularies::scv_domain_file){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_field
  source_col <- domain_filter$source_concept_field

  if(code_type=='source') {
     final_col = source_col
  }else if(code_type == 'cdm'){
    final_col = concept_col
  }else{cli::cli_abort(paste0(code_type, ' is not a valid argument. Please select either {.code source} or {.code cdm}'))}

  if(omop_or_pcornet == 'omop'){
    jc_col <- 'concept_id'
  }else if(omop_or_pcornet == 'pcornet'){jc_col <- 'concept_code'}

  join_cols <- set_names(jc_col, final_col)

  if(!is.na(domain_filter$vocabulary_field)){
    join_cols2 <- set_names('vocabulary_id', domain_filter$vocabulary_field)
    join_cols <- join_cols %>% append(join_cols2)
  }

  if(time){

    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain)) %>%
      filter(!!sym(domain_filter$date_field) >= start_date,
             !!sym(domain_filter$date_field) <= end_date) %>%
      filter(!!sym(domain_filter$date_field) >= time_start,
             !!sym(domain_filter$date_field) <= time_end)


    fact_tbl <-
      domain_tbl %>%
      inner_join(concept_set,
                 by=join_cols) %>%
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col),
             time_start,
             time_increment) %>%
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col) %>%
      group_by(time_start, time_increment, .add = TRUE)

  }else{

    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain)) %>%
      filter(!!sym(domain_filter$date_field) >= start_date,
             !!sym(domain_filter$date_field) <= end_date)


    fact_tbl <-
      domain_tbl %>%
      inner_join(concept_set,
                 by=join_cols) %>%
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col)) %>%
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col)

    }

  grouped_output <-
    fact_tbl %>%
    group_by(
      concept_id,
      source_concept_id,
      .add = TRUE
    ) %>% summarise(ct=n()) %>%
    collect()


  denom_concepts <-
    fact_tbl %>%
    group_by(
      concept_id,
      .add = TRUE
    ) %>% summarise(denom_concept_ct=n()) %>%
    collect()

  denom_source <-
    fact_tbl %>%
    group_by(
      source_concept_id,
      .add = TRUE
    ) %>% summarise(denom_source_ct=n()) %>%
  collect()

  grouped_output_totals <-
    grouped_output %>% left_join(denom_concepts) %>%
    left_join(denom_source) %>% #collect() %>%
    mutate(concept_prop = round(ct/denom_concept_ct, 2),
           source_prop = round(ct/denom_source_ct,2))



}



#' Compute Jaccard at the visit level
#'
#' This function will compute the Jaccard similarity index
#' at the visit level to determine how often two mappings occur
#' within the same visit when mapped to/from a given concept
#'
#' @param cohort  table of cohort members with at least `site`,
#'                a patient identifier, `start_date`, and `end_date`
#' @param domain_tbl a table with a list of domains and associated metadata; should include the name
#'                   of the domain, the name of the column where source codes can be found, the name
#'                   of the column where CDM codes can be found, and the date column that should
#'                   be used during over time computations
#' @param concept_set a csv file with the source or cdm codes of interest for the analysis.
#'                    should contain at least a `concept_id` column=
#' @param code_type the type of code to be examined in the check; either `source` or `cdm`
#' @param code_domain the domain related to the codes in the `concept_set`; should match a domain and
#'                    its associated metadata in the `domain_tbl` file
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#'
#' @returns a dataframe summarizing, for each code in the concept set, the jaccard
#'          similarity index of two mapped concepts within the same visit
#'
#' @importFrom tidyr pivot_longer
#'
compute_jaccard_scv <- function(cohort,
                                domain_tbl,
                                concept_set,
                                code_type,
                                code_domain,
                                omop_or_pcornet) {

  match_class <- function(x, type = var_class) {class(x) <- type; x}

  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_field
  source_col <- domain_filter$source_concept_field

  if(code_type=='source') {
    final_col = source_col
    filter_col = 'source_concept_id'
    var_col = 'concept_id'
  }else if(code_type == 'cdm'){
    final_col = concept_col
    filter_col = 'concept_id'
    var_col = 'source_concept_id'
  }

  if(omop_or_pcornet == 'omop'){
    jc_col <- 'concept_id'
    visit_col <- 'visit_occurrence_id'
  }else if(omop_or_pcornet == 'pcornet'){
    jc_col <- 'concept_code'
    visit_col <- 'encounterid'
  }

  join_cols <- set_names(jc_col, final_col)

  if(!is.na(domain_filter$vocabulary_field)){
    join_cols2 <- set_names('vocabulary_id', domain_filter$vocabulary_field)
    join_cols <- join_cols %>% append(join_cols2)
  }

  domain_tbl <- cohort %>%
    inner_join(cdm_tbl(code_domain)) %>%
    filter(!!sym(domain_filter$date_field) >= start_date,
           !!sym(domain_filter$date_field) <= end_date)

  jaccard_input_tbl <- domain_tbl %>%
    inner_join(concept_set,
               by=join_cols) %>%
    select(all_of(group_vars(cohort)),
           all_of(concept_col),
           all_of(source_col),
           all_of(visit_col)) %>%
    rename('concept_id' = concept_col,
           'source_concept_id' = source_col,
           'visit_id' = visit_col) %>% compute_new()

  concept_list <- jaccard_input_tbl %>% ungroup() %>% distinct(!!sym(filter_col)) %>%
    collect() %>% pull()

  opt_list <- list()

  for(i in concept_list){
    visits_concepts <-
      jaccard_input_tbl %>% ungroup %>%
      filter(!!sym(filter_col) == i) %>%
      select(visit_id,
             var_col) %>% distinct() %>% collect()

    var_class <- class(visits_concepts[[var_col]])

    visits_concepts_cts <-
      visits_concepts %>%
      group_by(!!sym(var_col)) %>%
      summarise(var_visit_ct=n_distinct(visit_id))

    concord <-
      visits_concepts %>% table() %>% crossprod()
    diag(concord) <- -1

    best <- as_tibble(concord, rownames='concept1') %>%
      pivot_longer(!concept1, names_to = 'concept2', values_to='cocount') %>%
      filter(cocount != -1L) %>% mutate(across(.cols = c(concept1, concept2), .fns=match_class)) %>%
      mutate(cocount = as.integer(cocount)) %>%
      left_join(visits_concepts_cts, by = c('concept1'=var_col))%>%
      rename(concept1_ct=var_visit_ct)%>%
      left_join(visits_concepts_cts, by = c('concept2'=var_col))%>%
      rename(concept2_ct=var_visit_ct) %>%
      mutate(concept_count_union=concept1_ct+concept2_ct-cocount,
             jaccard_index=cocount/concept_count_union) %>%
      mutate(concept1_prop=round(cocount/concept1_ct,2),
             concept2_prop=round(cocount/concept2_ct,2)) %>%
      filter(concept1_ct > 0 & concept2_ct > 0 & cocount > 0) %>%
      filter(concept1 > concept2)

    opt_list[[paste0(i)]] <- best %>% mutate(!!filter_col := i)

  }

  opt_list_red <- purrr::reduce(.x = opt_list,
                                .f = dplyr::union)

  return(opt_list_red)
}
