
#' SCV Output Generation
#'
#' @param process_output the output of the `scv_process` function
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv
#'                        file that is output to the provided results folder after running the `scv_process` function
#' @param code_type the type of code that is being used in the analysis, either `source` or `cdm`
#'
#'                  should ideally match the code_type that was defined when running `scv_process`
#' @param facet the variables by which you would like to facet the graph. available and/or recommended options for
#'              faceting variables are provided in the `parameter_summary` csv file
#' @param filter_concept for `scv_ms_anom_at` only -- choose ONE concept_id from the concept_set provided in
#'                       `scv_process` to filter the output
#' @param filter_mapped for `scv_ms_anom_at` only -- choose ONE mapped concept from those associated with the
#'                      concept_id provided in @filter_concept; options can be found in the `mapped_id` column
#'                      of `scv_process`
#' @param num_codes the number of top codes of code_type that should be displayed in the analysis
#'
#'                  used for `ss_exp_nt` and `ms_exp_nt`
#' @param num_mappings the number of top mappings that should be displayed for each code of code_type
#'
#'                     used for `ss_exp_nt`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#'
#' @return a graph to visualize the results from `scv_process` based on the parameters provided
#'
#'         in some cases, an additional reference table with summary information about the codes
#'         included in the graph
#'
#' @export
#'
scv_output <- function(process_output,
                       output_function,
                       code_type,
                       facet = NULL,
                       filter_concept = NULL,
                       filter_mapped = NULL,
                       num_codes = 10,
                       num_mappings = 25,
                       vocab_tbl = vocabulary_tbl('concept')){

  if(output_function != 'scv_ss_anom_at'){
    rslt_cid <- join_to_vocabulary(tbl = process_output,
                                   vocab_tbl = vocab_tbl,
                                   col = 'concept_id')

    rslt_scid <- join_to_vocabulary(tbl = process_output,
                                    vocab_tbl = vocab_tbl,
                                    col = 'source_concept_id') %>%
      rename('source_concept_name' = 'concept_name')

    process_output <- process_output %>%
      left_join(rslt_cid %>% distinct(concept_id, concept_name)) %>%
      left_join(rslt_scid %>% distinct(source_concept_id, source_concept_name))

  }else{
    process_output <- process_output
  }

  ## Run output functions
  if(output_function == 'scv_ms_anom_nt'){
    scv_output <- scv_ms_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 filter_concept = filter_concept,
                                 num_mappings = num_mappings)
  }else if(output_function == 'scv_ss_anom_nt'){
    scv_output <- scv_ss_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 num_codes = num_codes,
                                 num_mappings = num_mappings)
  }else if(output_function == 'scv_ms_exp_nt'){
    scv_output <- scv_ms_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes)
  }else if(output_function == 'scv_ss_exp_nt'){
    scv_output <- scv_ss_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                num_mappings = num_mappings)
  }else if(output_function == 'scv_ms_anom_at'){
    scv_output <- scv_ms_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 filter_concept = filter_concept,
                                 filter_mapped = filter_mapped)
  }else if(output_function == 'scv_ss_anom_at'){
    scv_output <- scv_ss_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 filter_concept = filter_concept,
                                 facet = facet)
  }else if(output_function == 'scv_ms_exp_at'){
    scv_output <- scv_ms_exp_at(process_output = process_output,
                                code_type = code_type,
                                filter_concept = filter_concept,
                                num_mappings = num_mappings,
                                facet = facet)
  }else if(output_function == 'scv_ss_exp_at'){
    scv_output <- scv_ss_exp_at(process_output = process_output,
                                code_type = code_type,
                                num_mappings = num_mappings,
                                facet = facet)
  }else(cli::cli_abort('Please enter a valid output function for this check type.'))

  return(scv_output)

}
