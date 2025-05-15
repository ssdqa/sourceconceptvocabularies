
#' Source and Concept Vocabularies
#'
#' This is a concept-set testing module that will compute frequency distributions for the usage of either
#' source-to-concept or concept-to-source concept pairs in order to highlight mapping patterns and impacts
#' of concept standardization. The user will provide the domain definitions (`domain_tbl`) and a concept
#' set with the concepts of interest (`concept_set`). Sample versions of these inputs are included as data
#' in the package and are accessible with `sourceconceptvocabularies::`. Results can optionally be stratified
#' by site, age group, and/or time. This function is compatible with both the OMOP and the PCORnet CDMs
#' based on the user's selection.
#'
#' @param cohort A dataframe with the cohort of patients for your study. Should include the columns:
#' - `person_id`
#' - `start_date`
#' - `end_date`
#' - `site`
#' @param concept_set for analyses where `time = FALSE`, a csv file with the source **OR** cdm codes of interest for the analysis.
#'
#'                    for analyses where `time = TRUE`, a vector with up to 5 source **OR** cdm codes of interest for the analysis.
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' @param domain_tbl a csv file that defines the domains where facts should be identified. defaults to the provided
#'                     `scv_domain_file`, which contains the following fields:
#' - `domain`: the CDM table where information for this domain can be found (i.e. drug_exposure)
#' - `concept_field`: the column in the CDM table where `cdm` codes can be identified (i.e. drug_concept_id or dx)
#' - `source_concept_field`: the column in the CDM table where `source` codes can be identified (i.e. drug_source_concept_id or raw_dx)
#' - `date_field`: the column in the CDM table that should be used as the default date field for
#' over time analyses (i.e. drug_exposure_start_date or dx_date)
#' - `vocabulary_field`: (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
#' @param code_type the type of code that is being used in the analysis, either `source` or `cdm`
#' @param code_domain the domain where the codes in the concept set should be searched for; must match
#'                    a domain defined in `domain_tbl`
#' @param jaccard_index **FOR `scv_ss_anom_cs` ONLY**: a boolean indicating whether a jaccard index
#'                      should be computed at the visit level to determine how often two mapped concepts
#'                      cooccur; can help identify potential post-coordination for SNOMED concepts
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#' - `single` - run the function for a single site
#' - `multi` - run the function for multiple sites
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param age_groups If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age`: the minimum age for the group (i.e. 10)
#' - `max_age`: the maximum age for the group (i.e. 20)
#' - `group`: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe with counts and proportions for each source -> cdm or cdm -> source mapping
#'         pair for each of the codes provided in `concept_set` this output should then be used in
#'         the `scv_output` function to generate an appropriate visualization
#'
#' @import dplyr
#' @import argos
#' @import squba.gen
#' @import cli
#' @importFrom cli cli_inform
#' @importFrom stringr str_wrap
#' @importFrom purrr reduce
#'
#' @example inst/example-scv_process_output.R
#'
#' @export
#'
scv_process <- function(cohort,
                        concept_set,
                        omop_or_pcornet,
                        domain_tbl=sourceconceptvocabularies::scv_domain_file,
                        code_type,
                        code_domain,
                        jaccard_index = FALSE,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        p_value = 0.9,
                        age_groups = NULL,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}
  if(!tolower(omop_or_pcornet) %in% c('omop', 'pcornet')){cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: please enter either {.code omop} or {.code pcornet}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'scv',
                                             as.list(environment())))


  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  # Set up grouped list

  grouped_list <- grouped_list %>% append('domain')

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}

  site_output <- list()

  # Prep cohort
    cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter,
                                  age_groups = age_groups, codeset = NULL,
                                  omop_or_pcornet = omop_or_pcornet) %>%
      mutate(domain = code_domain) %>%
      group_by(!!! syms(grouped_list))

  if(jaccard_index == TRUE && multi_or_single_site == 'single' &&
       anomaly_or_exploratory == 'anomaly' && time == FALSE){

    # Prep concept set for joins
    concept_set <- concept_set %>% collect()
    concept_set <- copy_to_new(df = concept_set)

    scv_tbl_final <- compute_jaccard_scv(cohort = cohort_prep,
                                         domain_tbl = domain_tbl,
                                         concept_set = concept_set,
                                         code_type = code_type,
                                         code_domain = code_domain,
                                         omop_or_pcornet = omop_or_pcornet)

    site_nm <- cohort_prep %>% ungroup() %>% collect() %>%
      distinct(!!sym(site_col)) %>% pull()

    scv_tbl_final <- scv_tbl_final %>% mutate(site = site_nm)

  }else{

    # Execute function
    if(! time) {

      # Prep concept set for joins
      concept_set <- concept_set %>% collect()
      concept_set <- copy_to_new(df = concept_set)

      for(k in 1:length(site_list_adj)) {

        site_list_thisrnd <- site_list_adj[[k]]

        # filters by site
        cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

        domain_compute <- check_code_dist(cohort = cohort_site,
                                          code_type = code_type,
                                          omop_or_pcornet = omop_or_pcornet,
                                          code_domain = code_domain,
                                          concept_set = concept_set,
                                          domain_tbl = domain_tbl)

        site_output[[k]] <- domain_compute

      }

      scv_tbl <- reduce(.x=site_output,
                        .f=dplyr::union)

      if(anomaly_or_exploratory == 'anomaly'){

        prop_col <- ifelse(code_type == 'cdm', 'concept_prop', 'source_prop')
        var_col <-  ifelse(code_type == 'cdm', 'concept_id', 'source_concept_id')
        denom_col <- ifelse(code_type == 'cdm', 'denom_concept_ct', 'denom_source_ct')

        if(multi_or_single_site == 'single'){

          scv_tbl_int <- compute_dist_anomalies(df_tbl = scv_tbl %>% replace_site_col(),
                                                grp_vars = c('domain', var_col),
                                                var_col = prop_col,
                                                denom_cols = c(var_col, denom_col))

          scv_tbl_final <- detect_outliers(df_tbl = scv_tbl_int,
                                           tail_input = 'both',
                                           p_input = p_value,
                                           column_analysis = prop_col,
                                           column_variable = c('domain', var_col))

        }else{
          scv_tbl_int <- compute_dist_anomalies(df_tbl = scv_tbl %>% replace_site_col(),
                                                grp_vars = c('domain', 'source_concept_id', 'concept_id'),
                                                var_col = prop_col,
                                                denom_cols = c(var_col, denom_col))

          scv_tbl_final <- detect_outliers(df_tbl = scv_tbl_int,
                                           tail_input = 'both',
                                           p_input = p_value,
                                           column_analysis = prop_col,
                                           column_variable = c('concept_id', 'source_concept_id'))
        }

      }else{scv_tbl_final <- scv_tbl}

    } else if(time){

      if(nrow(collect(concept_set)) > 5){cli::cli_abort('For an over time output, please filter your concept set to select 1-5
                                                          codes of interest')}

      concept_set <- concept_set %>% collect()
      concept_set <- copy_to_new(df = concept_set)

      scv_tbl <- compute_fot(cohort = cohort_prep,
                             site_col = site_col,
                             site_list = site_list_adj,
                             time_span = time_span,
                             time_period = time_period,
                             reduce_id = NULL,
                             check_func = function(dat){
                               check_code_dist(cohort = dat,
                                               concept_set = concept_set,
                                               omop_or_pcornet = omop_or_pcornet,
                                               code_type = code_type,
                                               code_domain = code_domain,
                                               domain_tbl = domain_tbl,
                                               time = TRUE)
                             })

      if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){

        var_col <- ifelse(code_type == 'cdm', 'concept_prop', 'source_prop')

        scv_tbl_final <- ms_anom_euclidean(fot_input_tbl = scv_tbl,
                                           grp_vars = c('site', 'concept_id', 'source_concept_id'),
                                           var_col = var_col)

      }else if(multi_or_single_site == 'single' && anomaly_or_exploratory == 'anomaly'){

        var_col <- ifelse(code_type == 'cdm', 'concept_id', 'source_concept_id')
        time_inc <- scv_tbl %>% ungroup() %>% distinct(time_increment) %>% pull()

        if(time_inc != 'year'){

        n_mappings_time <- scv_tbl %>%
          group_by(!!sym(var_col), time_start, time_increment) %>%
          summarise(n_mappings = n())

        scv_tbl_final <- anomalize_ss_anom_la(fot_input_tbl = n_mappings_time,
                                              time_var = 'time_start',
                                              grp_vars = var_col,
                                              var_col = 'n_mappings')
        }else{
          scv_tbl_final <- scv_tbl
        }

      }else{(scv_tbl_final <- scv_tbl)}

    }
  }

  cli::cli_inform(paste0(col_green('Based on your chosen parameters, we recommend using the following
                       output function in scv_output: '), col_blue(style_bold(output_type,'.'))))

  return(scv_tbl_final %>% replace_site_col())
}

