
#'
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats mad
#' @import ggplot2
#' @import gt
#' @import ggiraph
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom qicharts2 qic
#' @importFrom timetk plot_anomalies
#' @importFrom timetk plot_anomalies_decomp
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom graphics text
#' @importFrom tidyr unite
#' @importFrom rlang :=
NULL

#' Compute number and median mappings per code
#'
#' @param tbl intermediate table generated in the output function that contains the concepts
#'            of interest to be used to compute number of mappings
#' @param col the name of the column with the concept that needs to be summarized
#' @param denom the denominator count associated with @col
#' @param facet grouping variables to be used to compute group-specific counts and distance
#'              from overall median
#'
#' @return dataframe that summarizes the overall median number of mappings, group-specific number of
#'         mappings, and how many MAD from the overall median a code falls
#'
compute_mappings_per_code <- function(tbl,
                                      col,
                                      denom,
                                      facet = NULL){

  mappings_total <- tbl %>%
    group_by(!!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75)) %>%
    select(col, median, q1, q3) %>% ungroup() %>%
    left_join(tbl %>% distinct(!!sym(col), !!sym(denom)))

  mappings_group <- tbl %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    inner_join(mappings_total) %>%
    distinct() %>%
    mutate(mad = mad(n_mappings, center = median)) %>%
    ungroup() %>%
    mutate(dist_median = abs(n_mappings - median),
           n_mad = dist_median/mad)

  return(mappings_group)
}


#' *Single Site, Exploratory, Cross-Sectional*
#'
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#' @param num_mappings the number of top mappings that should be displayed for each code
#'
#' @return a heatmap with one facet per code and additional facet groupings, limited to the number
#'         of codes selected with @num_codes
#'         mapped code along the y-axis and proportion of representation as the fill
#' @return a reference table with additional information about the codes used as the facet.
#'         includes the code and denominator count, and will also include the concept name
#'         if @vocab_tbl is not NULL
#'
scv_ss_exp_cs <- function(process_output,
                          code_type,
                          facet = NULL,
                          num_codes = 10,
                          num_mappings = 10){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  # picking columns / titles
  if(code_type == 'cdm'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    name_col <- 'concept_name'
    map_name <- 'source_concept_name'
    prop <- 'concept_prop'
    title <- paste0('Top ', num_mappings, ' Mappings for Top ', num_codes, ' CDM Codes')
  }else if(code_type == 'source'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    name_col <- 'source_concept_name'
    map_name <- 'concept_name'
    prop <- 'source_prop'
    title <- paste0('Top ', num_mappings, ' Mappings for Top ', num_codes, ' Source Codes')
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}


  if(num_codes > 12){cli::cli_abort('Please only select up to 12 codes to maintain readability in the output.')}


  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    group_by(!!! syms(facet)) %>%
    arrange(desc(!! sym(denom))) %>%
    slice(1:num_codes)

  ref <- process_output %>%
    ungroup() %>%
    inner_join(topcodes) %>%
    distinct()

  nmap_total <- ref %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    summarise(nmap = n())

  nmap_top <- ref %>%
    select(col, map_col, all_of(facet), prop) %>%
    distinct() %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:num_mappings)

    final <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total) %>%
      rename(cname = !!map_name) %>%
      mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap),
             tooltip = paste0('Mapped Concept Name: ', cname))

    facet <- facet %>% append('xaxis')

    ## ggiraph interactive
    plt <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)),
                                 fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = tooltip)) +
      geom_text(aes(label = !!sym(prop)), size = 3, color = 'black') +
      scale_fill_squba(palette = 'diverging', discrete = FALSE) +
      facet_wrap((facet), scales = 'free') +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            text = element_text(size = 9)) +
      labs(title = title,
           x = col,
           y = map_col)

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)

    # Summary Reference Table
    ref_tbl <- generate_ref_table(tbl = final,
                                  id_col = col,
                                  name_col = name_col,
                                  denom = denom)

  output <- list(plt, ref_tbl)

  return(output)
}


#' *Multi Site, Exploratory, Cross-Sectional*
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#'
#' @return a searchable and filterable table with mappings, proportion of representation, and
#'         denominator counts for the number of codes selected
#'         in @num_codes
#'         concept name will be included if @vocab_tbl is not NULL
#'
scv_ms_exp_cs <- function(process_output,
                          code_type,
                          facet = NULL,
                          num_codes = 10){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  # picking columns / titles
  if(code_type == 'cdm'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    name_col <- 'concept_name'
    prop <- 'concept_prop'
  }else if(code_type == 'source'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    name_col <- 'source_concept_name'
    prop <- 'source_prop'
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}

  ## Enfore site facetting
  facet <- facet %>% append('site') %>% unique()

  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    group_by(!!! syms(facet)) %>%
    arrange(desc(!! sym(denom))) %>%
    slice(1:num_codes)


    final <- process_output %>%
      inner_join(topcodes) %>% distinct()

    table <- final %>%
      ungroup() %>%
      select(all_of(facet), col, map_col, concept_name, source_concept_name, ct, prop) %>%
      mutate(pct = !!sym(prop)) %>%
      arrange(!!!syms(facet), desc(ct)) %>%
      gt::gt() %>%
      cols_nanoplot(columns = pct, plot_type = 'bar',
                    autohide = TRUE, new_col_label = 'percent') %>%
      #gtExtras::gt_plt_bar_pct(column = pct) %>%
      fmt_number(columns = ct, decimals = 0) %>%
      fmt_percent(columns = prop, decimals = 0) %>%
      data_color(palette = squba_colors_standard, columns = c(all_of(facet))) %>%
      tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) %>%
      opt_interactive(use_search = TRUE,
                      use_filters = TRUE)

    return(table)

}

#'
#' *Single Site, Anomaly, Cross-Sectional*
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param filter_concept For instances when the jaccard_index analysis was executed,
#'                       the code_type concept_id of interest for which to display its mappings
#' @param facet the variables by which you would like to facet the graph
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#' @param num_mappings the number of top mappings that should be displayed for each code
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of usage
#'         for a cdm/source concept pair, and the size of the dot represents the
#'         mean proportion for the code type along the x axis
#'
scv_ss_anom_cs <- function(process_output,
                           code_type,
                           facet = NULL,
                           filter_concept = NULL,
                           num_codes = 10,
                           num_mappings = 15){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    denom <- 'denom_source_ct'
    prop <- 'source_prop'
    name_col <- 'source_concept_name'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    denom <- 'denom_concept_ct'
    prop <- 'concept_prop'
    name_col <- 'concept_name'
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}

  if('jaccard_index' %in% colnames(process_output)){

      tbl_grph <- process_output %>%
        rename('primary_col' := !!sym(col)) %>%
        filter(primary_col == filter_concept) %>%
        mutate(tooltip = paste0('Site: ', site,
                                '\nConcept 1: ', concept1_name,
                                '\nConcept 2: ', concept2_name,
                                '\nJaccard Index: ', jaccard_index))

      plt <- tbl_grph %>%
        ggplot(aes(x = as.character(concept1), y = as.character(concept2),
                   fill = jaccard_index, tooltip = tooltip)) +
        geom_tile_interactive() +
        scale_fill_squba(discrete = FALSE, palette = 'diverging',
                         limits = c(0,1)) +
        theme_minimal() +
        labs(title = paste0('Co-Occurrence of ', filter_concept, ' Mappings in Same Visit'),
             x = 'Concept 1',
             y = 'Concept 2',
             fill = 'Jaccard Index')

      plt[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                                   'tooltip' = TRUE)


  }else{

    cname_samp <- process_output %>% head(1) %>% select(concept_name) %>% pull()

    if(cname_samp == 'No vocabulary table input'){
      concept_label <- 'concept_id'
      source_concept_label <- 'source_concept_id'
    }else{concept_label <- 'concept_name'
          source_concept_label <- 'source_concept_name'}

    comparison_col <- prop

    topcodes <- process_output %>%
      #filter(anomaly_yn != 'no outlier in group') %>%
      ungroup() %>%
      select(col, denom, all_of(facet)) %>%
      distinct() %>%
      group_by(!!! syms(facet)) %>%
      arrange(desc(!! sym(denom))) %>%
      slice(1:num_codes)

    ref <- process_output %>%
      ungroup() %>%
      inner_join(topcodes) %>%
      distinct()

    nmap_total <- ref %>%
      group_by(!!sym(col), !!!syms(facet)) %>%
      summarise(nmap = n())

    nmap_top <- ref %>%
      select(col, map_col, all_of(facet), prop) %>%
      distinct() %>%
      group_by(!!sym(col), !!!syms(facet)) %>%
      arrange(desc(!!sym(prop))) %>%
      slice(1:num_mappings)

    final <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total)

    dat_to_plot <- final %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      mutate(text=paste("Concept: ", !!sym(concept_label),
                        "\nSource Concept: ", !!sym(source_concept_label),
                        "\nSite: ",site,
                        "\nProportion: ",round(!!sym(comparison_col),2),
                        "\nMean proportion:",round(mean_val,2),
                        '\nSD: ', round(sd_val,2),
                        "\nMedian proportion: ",round(median_val,2),
                        "\nMAD: ", round(mad_val,2))) %>%
      mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))


    #mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2

    plt<-ggplot(dat_to_plot,
                aes(x=!!sym(col), y=!!sym(map_col), text=text, color=!!sym(comparison_col)))+
      geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
      geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                             aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
      scale_color_squba(palette = 'diverging', discrete = FALSE) +
      scale_shape_manual(values=c(19,8))+
      scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
      theme_minimal() +
      # theme(axis.text.x = element_text(angle=60, hjust = 1)) +
      labs(size="",
           title=paste0('Anomalous Concept Pairs'),
           subtitle = paste0('For Top ', num_codes, ' Codes and Top ', num_mappings, ' Mappings \nDot size is the mean proportion')) +
      guides(color = guide_colorbar(title = 'Proportion'),
             shape = guide_legend(title = 'Anomaly'),
             size = 'none')


    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)
  }

  return(plt)

}


#' *Multi-Site, Anomaly, Cross-Sectional*
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param filter_concept the code_type concept_id of interest for which to display its mappings
#' @param num_mappings an integer indicating the number of top mappings that should be displayed
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of representation
#'         of a filter_concept/mapping concept pair, and the size of the dot
#'         represents the mean proportion across all sites
#'
scv_ms_anom_cs <- function(process_output,
                           code_type,
                           facet = NULL,
                           filter_concept,
                           num_mappings = 20){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    prop <- 'source_prop'
    name_col <- 'concept_name'
    title_col <- 'source_concept_name'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    prop <- 'concept_prop'
    name_col <- 'source_concept_name'
    title_col <- 'concept_name'
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}

  cname_samp <- process_output %>% head(1) %>% select(concept_name) %>% pull()

  if(cname_samp == 'No vocabulary table input'){
    concept_label <- map_col
  }else{concept_label <- name_col}

  comparison_col <- prop

  nmap_top <- process_output %>%
    filter(!!sym(col) == filter_concept) %>%
    select(col, map_col, all_of(facet), prop) %>%
    distinct() %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:num_mappings)

  final <- process_output %>%
    inner_join(nmap_top)

  check_n <- final %>%
    filter(anomaly_yn != 'no outlier in group')

  dat_to_plot <- final %>%
    mutate(concept_id = as.character(concept_id),
           source_concept_id = as.character(source_concept_id)) %>%
    mutate(text=paste("Mapped Concept: ", !!sym(concept_label),
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2))) %>%
    mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

  title_name <- process_output %>% filter(!!sym(col) == filter_concept) %>%
    distinct(!!sym(title_col)) %>% pull()

  if(nrow(check_n) > 0){

    plt<-ggplot(dat_to_plot,
                aes(x=site, y=!!sym(map_col), text=text, color=!!sym(comparison_col)))+
      geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
      geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                             aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
      scale_color_squba(palette = 'diverging', discrete = FALSE) +
      scale_shape_manual(values=c(19,8))+
      scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=60, hjust = 1)) +
      labs(size="",
           title=paste0('Anomalous Mappings for ', filter_concept, ' : ', title_name),
           subtitle = paste0('From Top ', num_mappings, ' Mappings \nDot size is the mean proportion per mapped concept')) +
      guides(color = guide_colorbar(title = 'Proportion'),
             shape = guide_legend(title = 'Anomaly'),
             size = 'none')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)

    return(plt)

  }else{
    plt <- ggplot(dat_to_plot, aes(x = site, y = !!sym(map_col), fill = !!sym(comparison_col),
                                   tooltip = text)) +
      geom_tile_interactive() +
      theme_minimal() +
      scale_fill_squba(discrete = FALSE, palette = 'diverging') +
      labs(x = 'Site',
           title = paste0(filter_concept, ' : ', title_name))

    # Test Site Score using SD Computation
    test_site_score <- process_output %>%
      mutate(dist_mean = (!!sym(comparison_col) - mean_val)^2) %>%
      group_by(site) %>%
      summarise(n_grp = n(),
                dist_mean_sum = sum(dist_mean),
                overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nStandard Deviation: ', round(overall_sd, 3)))

    ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
    ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1

    g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                      tooltip = tooltip)) +
      geom_point_interactive(show.legend = FALSE) +
      theme_minimal() +
      scale_color_squba() +
      geom_hline(yintercept = 0, linetype = 'solid') +
      #geom_hline(yintercept = 1, linetype = 'dotted', color = 'gray', linewidth = 1) +
      #geom_hline(yintercept = -1, linetype = 'dotted', color = 'gray', linewidth = 1) +
      #ylim(ylim_min, ylim_max) +
      labs(title = 'Average Standard Deviation per Site',
           y = 'Average Standard Deviation',
           x = 'Site')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)
    g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

    opt <- list(plt,
                g2)

    return(opt)
  }

}


#' *Single Site, Exploratory, Longitudinal*
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param num_mappings integer indicating the number of top mappings per code to be
#'                     displayed in the output
#' @param facet the variables by which you would like to facet the graph
#'
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#'
scv_ss_exp_la <- function(process_output,
                          code_type,
                          num_mappings = 10,
                          facet = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    name_col <- 'source_concept_name'
    map_name <- 'concept_name'
    prop <- 'source_prop'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    name_col <- 'concept_name'
    map_name <- 'source_concept_name'
    prop <- 'concept_prop'
    denom <- 'denom_concept_ct'
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}

  nmap_top <- process_output %>%
    select(col, map_col, all_of(facet), prop) %>%
    distinct() %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:num_mappings)

  final <- process_output %>%
    inner_join(nmap_top)

  facet <- facet %>% append(col) %>% unique()

    p <- final %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(map_col),
                 label = !!sym(map_name),
                 label2 = ct
                 )) +
      geom_line() +
      scale_color_squba() +
      facet_wrap((facet)) +
      theme_minimal() +
      labs(title = paste0('Top ', num_mappings, ' Mapping Pairs Over Time'),
           color = map_col)

    p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = FALSE)

    ref_tbl <- generate_ref_table(tbl = process_output,
                                  id_col = col,
                                  name_col = name_col,
                                  denom = denom,
                                  time = TRUE)

  output <- list(p, ref_tbl)

  return(output)

}

#' *Multi Site, Exploratory, Longitudinal*
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param filter_concept the code_type concept of interest for which mappings should be shown
#' @param num_mappings an integer indicating the top number of mappings for filter_concept
#'                     that should be displayed
#' @param facet the variables by which you would like to facet the graph
#'
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#'
scv_ms_exp_la <- function(process_output,
                          code_type,
                          filter_concept,
                          num_mappings = 10,
                          facet = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    name_col <- 'source_concept_name'
    map_name <- 'concept_name'
    prop <- 'source_prop'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    name_col <- 'concept_name'
    map_name <- 'source_concept_name'
    prop <- 'concept_prop'
    denom <- 'denom_concept_ct'
  }else{cli::cli_abort('Please select a valid code_type: {.code source} or {.code cdm}')}

  nmap_top <- process_output %>%
    filter(!!sym(col) == filter_concept) %>%
    select(col, map_col, all_of(facet), ct) %>%
    group_by(!!sym(col), !!sym(map_col)) %>%
    summarise(ct = sum(ct)) %>%
    ungroup() %>%
    distinct() %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(ct)) %>%
    slice(1:num_mappings) %>%
    distinct(!!sym(col), !!sym(map_col))

  final <- process_output %>%
    inner_join(nmap_top)

  facet <- facet %>% append(map_col) %>% unique()

  p <- final %>%
    mutate(concept_id = as.character(concept_id),
           source_concept_id = as.character(source_concept_id)) %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = site,
               label = !!sym(map_name),
               label2 = ct
    )) +
    geom_line() +
    scale_color_squba() +
    facet_wrap((facet)) +
    theme_minimal() +
    labs(title = paste0('Top ', num_mappings, ' Mappings for ', filter_concept, ' Over Time'),
         color = 'Site')

  p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = FALSE)

  ref_tbl <- generate_ref_table(tbl = process_output %>% ungroup(),
                                id_col = map_col,
                                name_col = map_name,
                                denom = denom,
                                time = TRUE)

  output <- list(p, ref_tbl)

  return(output)

}

#' **Multi-Site, Anomaly, Longitudinal**
#'
#' @param process_output output from `evp_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param filter_concept the code_type concept of interest to be displayed in the output
#' @param filter_mapped the mapped concept of interest to be displayed in the output
#'
#'
#' @return three graphs:
#'    1) line graph that shows the smoothed proportion of a
#'    concept pair across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of a
#'    concept pair across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#'
#' THIS GRAPH SHOWS ONLY ONE CONCEPT PAIR AT A TIME!
#'

scv_ms_anom_la <- function(process_output,
                           code_type,
                           filter_concept,
                           filter_mapped) {

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'cdm'){
    prop <- 'concept_prop'
    concept_col <- 'concept_id'
    mapped_col <- 'source_concept_id'
  }else if(code_type == 'source'){
    prop <- 'source_prop'
    concept_col <- 'source_concept_id'
    mapped_col <- 'concept_id'
  }else(cli::cli_abort('Please select a valid code type: {.code source} or {.code cdm}'))

  filt_op <- process_output %>% filter(!!sym(concept_col) == filter_concept,
                                       !!sym(mapped_col) == filter_mapped) %>%
    mutate(prop_col = !!sym(prop))

  allsites <-
    filt_op %>%
    select(time_start,!!sym(concept_col),!!sym(mapped_col),mean_allsiteprop) %>% distinct() %>%
    rename(prop_col=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop_col),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop_col))

  dat_to_plot <-
    filt_op %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_col,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_squba() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', filter_concept, ' - ', filter_mapped, ' Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    scale_color_squba() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', filter_concept, ' - ', filter_mapped, ' Across Time'))

  t <- dat_to_plot %>%
    distinct(site, dist_eucl_mean, site_loess) %>%
    group_by(site, dist_eucl_mean) %>%
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nEuclidean Distance: ', dist_eucl_mean,
                            '\nAverage Loess Proportion: ', mean_site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
    geom_col_interactive() +
    # geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
    #           show.legend = FALSE) +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() +
    scale_fill_squba(palette = 'diverging', discrete = FALSE) +
    # theme(legend.position = 'bottom',
    #       legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
    #       axis.text.x = element_text(face = 'bold')) +
    labs(fill = 'Avg. Proportion \n(Loess)',
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance for ', filter_concept, ' - ', filter_mapped))

  p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  output <- list(p,
                 q,
                 t)

  return(output)

}

#' *Single Site, Anomaly, Longitudinal*
#'
#'
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`;
#'                  should match the code_type provided when running `scv_process`
#' @param filter_concept the code_type concept of interest to be displayed in the output
#' @param facet the variables by which you would like to facet the graph
#'
#' @return if analysis was executed by year or greater, a C control chart
#'         showing the number of mappings for filter_concept
#'         is returned with outliers marked with orange dots
#'
#'         if analysis was executed by month or smaller, an STL regression is
#'         conducted and outliers are marked with red dots. the graphs representing
#'         the data removed in the regression are also returned
#'
scv_ss_anom_la <- function(process_output,
                           code_type,
                           filter_concept,
                           facet = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(code_type == 'source'){
    col <- 'source_concept_id'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
  }else{cli::cli_abort('Please choose a valid code_type: {.code source} or {.code cdm}')}

  time_inc <- process_output %>% filter(!is.na(time_increment)) %>%
    distinct(time_increment) %>% pull()

  facet <- facet %>% append(col)

  if(time_inc == 'year'){

    n_mappings_time <- process_output %>%
      group_by(!!!syms(facet), time_start, time_increment) %>%
      summarise(n_mappings = n()) %>%
      filter(!!sym(col) == filter_concept) %>%
      unite(facet_col, !!!syms(facet), sep = '\n')

  c_qi <- qic(data = n_mappings_time, x = time_start, y = n_mappings, chart = 'c', facets = ~facet_col,
      title = 'Control Chart: Number of Mappings per Code', ylab = '# of Mappings', xlab = 'Time',
      show.grid = TRUE)

  op_dat <- c_qi$data

  new_c <- ggplot(op_dat,aes(x,y)) +
    geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
    geom_line(colour = squba_colors_standard[[12]], size = .5) +
    geom_line(aes(x,cl)) +
    geom_point(colour = squba_colors_standard[[6]] , fill = squba_colors_standard[[6]], size = 1) +
    geom_point(data = subset(op_dat, y >= ucl), color = squba_colors_standard[[3]], size = 2) +
    geom_point(data = subset(op_dat, y <= lcl), color = squba_colors_standard[[3]], size = 2) +
    facet_wrap(~facet1) +
    theme_minimal() +
    ggtitle(label = 'Control Chart: Number of Mappings per Code') +
    labs(x = 'Time',
         y = '# of Mappings')+
    theme_minimal()

  new_c[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                'tooltip' = FALSE)

  output <- new_c

  }else{

    anomalies <-
      plot_anomalies(.data=process_output %>% filter(!!sym(col) == filter_concept),
                     .date_var=time_start,
                     .interactive = FALSE,
                     .title = paste0('Anomalous # of Mappings for ', filter_concept, ' Over Time')) #%>%
      #layout(title = paste0('Anomalous # of Mappings for ', filter_concept, ' Over Time'))

    decomp <-
      plot_anomalies_decomp(.data=process_output %>% filter(!!sym(col) == filter_concept),
                            .date_var=time_start,
                            .interactive = FALSE,
                            .title = paste0('Anomalous # of Mappings for ', filter_concept, ' Over Time')) #%>%
      #layout(title = paste0('Anomalous # of Mappings for ', filter_concept, ' Over Time'))

    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- list(anomalies, decomp)

  }

  return(output)
}
