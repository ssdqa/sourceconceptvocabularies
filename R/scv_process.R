
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
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param concept_set *tabular input or vector* || **required**
#'
#'   For analyses where `time = FALSE`, this input should be a table containing
#'   the standard CDM concepts **OR** source concepts of interest for the analysis.
#'   This input should contain at least one of following:
#'   - `concept_id` | *integer* | the concept_id of interest (required for OMOP)
#'   - `concept_code` | *character* | the code of interest (required for PCORnet)
#'
#'   For certain PCORnet applications, it should also contain
#'   - `vocabulary_id` | *character* | the vocabulary of the code, which should match what is listed in the domain table's `vocabulary_field`
#'
#'
#'   For analyses where `time = TRUE`, this input should be a vector with up to
#'   5 standard CDM **OR** source concepts of interest for the analysis. This limitation
#'   is applied to reduce computational strain. We recommend running a cross-sectional
#'   analysis first to identify potential concepts of interest, then using these as input
#'   for the longitudinal analysis.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param domain_tbl *tabular input* || **required**
#'
#'   A table that defines the domains where concepts should be identified. This
#'   input should contain:
#'   - `domain` | *character* | the name of the CDM table where the concepts can be identified
#'   - `concept_field` | *character* | the name of the field in the CDM table where standard `cdm` codes can be identified (i.e. drug_concept_id or dx)
#'   - `source_concept_field` | *character* |  the name of the field in the CDM table where `source` codes can be identified (i.e. drug_source_concept_id or raw_dx)
#'   - `date_field` | *character* | the name of the field in the CDM table that should be used for temporal filtering
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'
#'   To see an example of this input, see `?sourceconceptvocabularies::scv_domain_file`
#'
#' @param code_type *string* || **required**
#'
#'   A string identifying the type of concept that has been provided in the `concept_set`.
#'
#'   Acceptable values are `cdm` (the standard, mapped code that is included in the CDM) or `source` (the "raw" concept from the source system)
#'
#' @param code_domain *string* || **required**
#'
#'   The string name of the domain where the concepts can be identified. This input
#'   should match at least one of the domains in the `domain_tbl`, and it will function
#'   to filter this table down to only the relevant domain and allow the user to store
#'   multiple domains in this table for reuse in other analyses.
#'
#' @param jaccard_index *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether a Jaccard index should be computed at the
#'   visit level to determine how often two mapped concepts cooccur in the same encounter.
#'   This computation can help identify potential instances of post-coordination for SNOMED concepts.
#'
#'   This is only applicable for the `Single Site, Anomaly Detection, Cross-Sectional` check.
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param p_value *numeric* || defaults to `0.9`
#'
#'   The p value to be used as a threshold in the Multi-Site,
#'   Anomaly Detection, Cross-Sectional analysis
#'
#' @param anomaly_or_exploratory *string* | Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @return This function will return a dataframe summarizing the
#'         mapping patterns for each concept provided by the user. For a
#'         more detailed description of output specific to each check type,
#'         see the PEDSpace metadata repository
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

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                      '`scv_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                      'See ?scv_output for more details.'), padding = c(0,1,0,1),
                    header = cli::col_cyan('Output Function Details')))

  return(scv_tbl_final %>% replace_site_col() %>% mutate(output_function = output_type$string))
}

