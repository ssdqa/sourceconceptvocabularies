
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(scv_output(process_output = tbl_test,
                          output_function = 'scv_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a'),
                            domain = c('dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7),
                            ct = c(10,12,14,16,18,20),
                            denom_concept_ct = c(100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            output_function = c('scv_ss_exp_cs','scv_ss_exp_cs','scv_ss_exp_cs',
                                                'scv_ss_exp_cs','scv_ss_exp_cs','scv_ss_exp_cs'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL))

})


test_that('multi site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'b', 'b', 'b'),
                            domain = c('dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7),
                            ct = c(10,12,14,16,18,20),
                            denom_concept_ct = c(100,100,100,200,200,200),
                            denom_source_ct = c(50,55,60,65,70,75),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            output_function = c('scv_ms_exp_cs','scv_ms_exp_cs','scv_ms_exp_cs',
                                                'scv_ms_exp_cs','scv_ms_exp_cs','scv_ms_exp_cs'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL))

})


test_that('single site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a'),
                            domain = c('dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7),
                            ct = c(10,12,14,16,18,20),
                            denom_concept_ct = c(100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            'mean_val' = c(0.85, 0.85, 0.85,0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82,0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05,0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02,0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01,0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95,0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79,0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16,0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3,3,3,3),
                            'analysis_eligible' = c('yes','yes','yes','yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134,0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932,0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier',
                                             'no outlier', 'outlier', 'outlier'),
                            'output_function' = c('scv_ss_anom_cs','scv_ss_anom_cs','scv_ss_anom_cs',
                                                'scv_ss_anom_cs','scv_ss_anom_cs','scv_ss_anom_cs'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL))

})


test_that('single site, anomaly detection, no time jaccard', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a'),
                            domain = c('dx','dx','dx','dx','dx','dx'),
                            concept1 = c(2,3,4,5,6,7),
                            concept2 = c(3,4,5,3,7,2),
                            concept_id = c(1,1,1,1,1,1),
                            cocount = c(15, 20, 25, 30, 35, 40),
                            concept1_ct = c(10,15,20,25,30,35),
                            concept2_ct = c(15,20,25,25,35,10),
                            jaccard_index = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                            'output_function' = c('scv_ss_anom_cs','scv_ss_anom_cs','scv_ss_anom_cs',
                                                  'scv_ss_anom_cs','scv_ss_anom_cs','scv_ss_anom_cs'))

  expect_no_error(scv_output(process_output = tbl_test,
                             filter_concept = 1,
                             code_type = 'cdm',
                             vocab_tbl = NULL))

})


test_that('multi site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'b', 'b', 'b'),
                            domain = c('dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7),
                            ct = c(10,12,14,16,18,20),
                            denom_concept_ct = c(100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6),
                            'mean_val' = c(0.85, 0.85, 0.85,0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82,0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05,0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02,0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01,0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95,0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79,0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16,0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3,3,3,3),
                            'analysis_eligible' = c('yes','yes','yes','yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134,0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932,0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier',
                                             'no outlier', 'outlier', 'outlier'),
                            'output_function' = c('scv_ms_anom_cs','scv_ms_anom_cs','scv_ms_anom_cs',
                                                  'scv_ms_anom_cs','scv_ms_anom_cs','scv_ms_anom_cs'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

  expect_no_error(scv_output(process_output = tbl_test %>% mutate(anomaly_yn == 'no outlier in group'),
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

})


test_that('single site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01'),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year'),
                            domain = c('dx','dx','dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7,8,9),
                            ct = c(10,12,14,16,18,20,22,24),
                            denom_concept_ct = c(100,100,100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75,80,85),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            'output_function' = c('scv_ss_exp_la','scv_ss_exp_la','scv_ss_exp_la',
                                                  'scv_ss_exp_la','scv_ss_exp_la','scv_ss_exp_la',
                                                  'scv_ss_exp_la','scv_ss_exp_la'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

})

test_that('multi site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'b',
                                     'b', 'b', 'b'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01'),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year'),
                            domain = c('dx','dx','dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7,8,9),
                            ct = c(10,12,14,16,18,20,22,24),
                            denom_concept_ct = c(100,100,100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75,80,85),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            'output_function' = c('scv_ms_exp_la','scv_ms_exp_la','scv_ms_exp_la',
                                                  'scv_ms_exp_la','scv_ms_exp_la','scv_ms_exp_la',
                                                  'scv_ms_exp_la','scv_ms_exp_la'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

})


test_that('single site, anomaly detection, across time -- year', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01'),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year'),
                            domain = c('dx','dx','dx','dx','dx','dx','dx','dx'),
                            concept_id = c(1,1,1,1,1,1,1,1),
                            source_concept_id = c(2,3,4,5,6,7,8,9),
                            ct = c(10,12,14,16,18,20,22,24),
                            denom_concept_ct = c(100,100,100,100,100,100,100,100),
                            denom_source_ct = c(50,55,60,65,70,75,80,85),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            source_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                            'output_function' = c('scv_ss_anom_la','scv_ss_anom_la','scv_ss_anom_la',
                                                  'scv_ss_anom_la','scv_ss_anom_la','scv_ss_anom_la',
                                                  'scv_ss_anom_la','scv_ss_anom_la'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

})

test_that('single site, anomaly detection, across time -- month', {

  tbl_test <- tidyr::tibble(time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01','2022-01-01', '2022-01-01'),
                            time_increment = c('month', 'month', 'month', 'month', 'month',
                                               'month', 'month', 'month','month','month'),
                            concept_id = c(1,1,1,1,1,1,1,1,1,1),
                            n_mappings = c(10,12,14,16,18,20,22,24,26,28),
                            'observed' = c(0.5, 0.6, 0.7, 0.8, 0.9,
                                           0.5, 0.6, 0.7, 0.8, 0.9),
                            'season' = c(1,2,3,4,5,1,2,3,4,5),
                            'trend' = c(1,2,3,4,5,1,2,3,4,5),
                            'remainder' = c(0.46, 0.57, 0.69, 0.82, 0.88,
                                            0.46, 0.57, 0.69, 0.82, 0.88),
                            'seasonadj' = c(1,2,3,4,5,1,2,3,4,5),
                            'anomaly' = c('Yes', 'No', 'No', 'No', 'Yes',
                                          'Yes', 'No', 'No', 'No', 'Yes'),
                            'anomaly_direction' = c(-1,0,0,0,1,-1,0,0,0,1),
                            'anomaly_score' = c(1,2,3,4,5,1,2,3,4,5),
                            'recomposed_l1' = c(0.44, 0.6, 0.5, 0.49, 0.46,
                                                0.44, 0.6, 0.5, 0.49, 0.46),
                            'recomposed_l2' = c(0.84, 0.8, 0.8, 0.89, 0.86,
                                                0.84, 0.8, 0.8, 0.89, 0.86),
                            'observed_clean' = c(0.46, 0.57, 0.69, 0.82, 0.88,
                                                 0.46, 0.57, 0.69, 0.82, 0.88),
                            'output_function' = c('scv_ss_anom_la','scv_ss_anom_la','scv_ss_anom_la',
                                                  'scv_ss_anom_la','scv_ss_anom_la','scv_ss_anom_la',
                                                  'scv_ss_anom_la','scv_ss_anom_la','scv_ss_anom_la',
                                                  'scv_ss_anom_la'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1))

})


test_that('multi site, anomaly detection, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'b', 'b',
                                     'b', 'c', 'c', 'c'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01'),
                            concept_id = c(1,1,1,1,1,1,1,1,1),
                            source_concept_id = c(2,2,2,2,2,2,2,2,2),
                            concept_prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9,0.9,0.9,0.9),
                            'output_function' = c('scv_ms_anom_la','scv_ms_anom_la','scv_ms_anom_la',
                                                  'scv_ms_anom_la','scv_ms_anom_la','scv_ms_anom_la',
                                                  'scv_ms_anom_la','scv_ms_anom_la','scv_ms_anom_la'))

  expect_no_error(scv_output(process_output = tbl_test,
                             code_type = 'cdm',
                             vocab_tbl = NULL,
                             filter_concept = 1,
                             filter_mapped = 2))

})
