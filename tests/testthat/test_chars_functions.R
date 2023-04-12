library('data.table')
library('testthat')

# Check chars_icd_ccs() ----
  ccstable <- chars_icd_ccs()

  test_that("Check structure of returned table", {
    expect_identical(ccstable, chars_icd_ccs(ref_type = 'all'))

    expect_true(inherits(ccstable, 'data.table')) # confirm data.table
    expect_equal(sort(names(ccstable)), sort(c('icd10cm_code', 'icd10cm', 'level1', 'level2', 'level3')))
    expect_gt(nrow(ccstable), 71000)

    expect_equal(chars_icd_ccs(), chars_icd_ccs(ref_type = 'all'))
    expect_true(inherits(chars_icd_ccs(ref_type = 'icd10'), 'data.table'))
    expect_equal(sort(names(chars_icd_ccs(ref_type = 'icd10'))), c('icd10cm', 'icd10cm_code'))

    expect_true(inherits(chars_icd_ccs(ref_type = 'level1'), 'data.table'))
    expect_equal(names(chars_icd_ccs(ref_type = 'level1')), 'level1')

    expect_true(inherits(chars_icd_ccs(ref_type = 'level2'), 'data.table'))
    expect_equal(names(chars_icd_ccs(ref_type = 'level2')), 'level2')

    expect_true(inherits(chars_icd_ccs(ref_type = 'level3'), 'data.table'))
    expect_equal(names(chars_icd_ccs(ref_type = 'level3')), 'level3')

    expect_error(chars_icd_ccs(ref_type = 'blah'))

  })

# Check chars_icd_ccs_count() ----
  charsdata <- get_data_chars(year = 2019, cols = c("seq_no", "diag1", "chi_sex"), kingco = T)

  test_that("Function returns expected rows, columns, and values...", {
    # test icd10 argument ----
      icd.result <- chars_icd_ccs_count(ph.data = charsdata,
                                        level1 = NULL,
                                        level2 = NULL,
                                        level3 = NULL,
                                        icd10cm = '^Kidney transplant',
                                        icdcol = 'diag1',
                                        group_by = NULL,
                                        kingco = F)
      expect_equal(nrow(icd.result), 4)
      expect_equal(sort(names(icd.result)), sort(c('icd10cm_desc', 'hospitalizations')))
      expect_equal(sum(icd.result$hospitalizations), nrow(charsdata[grepl("T8611|T8612|T8613|Z940", x= diag1)]))

    # test level3 argument ----
      level3.result <- chars_icd_ccs_count(ph.data = charsdata, level3 = 'Chronic kidney disease', kingco = F)
      expect_equal(nrow(level3.result), 1)
      expect_equal(sort(names(level3.result)), sort(c('level3_desc', 'hospitalizations')))
      expect_equal(sum(level3.result$hospitalizations), 82) # confirmed vs CHAT

    # test level2 argument ----
      level2.result <- chars_icd_ccs_count(ph.data = charsdata, level2 = 'Cystic fibrosis', kingco = F)
      expect_equal(nrow(level2.result), 1)
      expect_equal(sort(names(level2.result)), sort(c('level2_desc', 'hospitalizations')))
      expect_equal(sum(level2.result$hospitalizations), 63) # confirmed vs CHAT

    # test level1 argument ----
      level1.result <- chars_icd_ccs_count(ph.data = charsdata, level1 = 'Diseases of the blood', kingco = F)
      expect_equal(nrow(level1.result), 1)
      expect_equal(sort(names(level1.result)), sort(c('level1_desc', 'hospitalizations')))
      expect_equal(sum(level1.result$hospitalizations), 1601) # confirmed vs CHAT

    # test group_by argument ----
      group_by.result <- chars_icd_ccs_count(ph.data = charsdata[chi_sex %in% c("Male", "Female")],
                                             level3 = 'Chronic kidney disease',
                                             kingco = F,
                                             group_by = 'chi_sex')
      expect_equal(nrow(group_by.result), 2)
      expect_equal(sort(names(group_by.result)), sort(c('chi_sex', 'level3_desc', 'hospitalizations')))
      expect_equal(sum(group_by.result$hospitalizations), 82) # confirmed vs CHAT

    # test value combination of search terms (and confirm search is case insensitive) ----
      multi.result <- chars_icd_ccs_count(ph.data = charsdata,
                                          icd10 = '^kidney transplant',
                                          level1 = '^injury and poisoning',
                                          level2 = '^complication',
                                          level3 = '^complication',
                                          kingco = F)
      expect_equal(nrow(multi.result), 3)
      expect_equal(sort(names(multi.result)),
                   sort(c('icd10cm_desc', 'level1_desc', 'level2_desc', 'level3_desc', 'hospitalizations')))
      expect_equal(sum(multi.result$hospitalizations), 86) # confirmed vs CHAT
  })

  test_that("Function gives errors as appropriate...", {
    # should error when no search strings given
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd10 = NULL, kingco = T))

    # should error when mis-specify the name of ph.data
    expect_error(chars_icd_ccs_count(ph.data = 'charsdata', icd10 = '^Kidney transplant', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = NULL, icd10 = '^Kidney transplant', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata2, icd10 = '^Kidney transplant', kingco = F))

    # should error because missing chi_geo_kc
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd10 = '^Kidney transplant', kingco = T))

    # should error because 'blah' is not a medical diagnosis!
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd10 = 'blah', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, level1 = 'blah', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, level2 = 'blah', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, level3 = 'blah', kingco = F))

    # should error when mis-specify the column containing icd10cm data
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd10 = '^Kidney transplant', kingco = F, icdcol = NULL))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd10 = '^Kidney transplant', kingco = F, icdcol = 'mycolumn'))

    # should error due to group_by because 'blah' is not a column in the dataset
    expect_error(chars_icd_ccs_count(ph.data = charsdata, level3 = 'Chronic kidney disease', kingco = F, group_by = 'blah'))

    # should error message when filter out all values at more granular level
    expect_error(chars_icd_ccs_count(ph.data = charsdata,
                                     icd10 = '^Kidney transplant',
                                     level1 = 'Injury and poisoning',
                                     level2 = 'Complication',
                                     level3 = 'Hernia',
                                     kingco = F))

    # should give warning when pass an icdcol that has non-standard values
    expect_warning(chars_icd_ccs_count(ph.data = charsdata, icd10 = '^Kidney transplant', kingco = F, icdcol = 'chi_sex'))
    })


# Check chars_injury_matrix() ----
  injurytable <- chars_injury_matrix()

  test_that("Check structure of returned table", {
    expect_true(inherits(injurytable, 'data.table')) # confirm data.table
    expect_equal(sort(names(injurytable)), sort(c('mechanism', 'intent')))
    expect_gte(nrow(injurytable), 100) # initial table is >= 100 rows long
    expect_lte(nrow(injurytable), 200) # imagine 200 rows would be too long, even if we add to the options
  })

# Check chars_injury_matrix_count() ----
  charsdt <- get_data_chars(year = 2019, kingco = T)

  test_that("Function returns expected rows, columns, and values...", {
  # test ph.data argument ----
    expect_error(chars_injury_matrix_count(ph.data = 'charsdt'))

  # test intent argument ----
    # check that that * gives all intent & mechanism by sampling some commonly of interest
    chars1 <- chars_injury_matrix_count(ph.data = charsdt, intent = '*')
    expect_true(sum(c('intentional', 'unintentional', 'legal', 'assault', 'Any intent') %in% unique(chars1$intent)) == 5)

    # confirm 'none' collapses the intent
    chars2 <- chars_injury_matrix_count(ph.data = charsdt, intent = 'none')
    expect_identical('Any intent', unique(chars2$intent))

    # confirm can select intent of interest
    chars3 <- chars_injury_matrix_count(ph.data = charsdt, intent = 'assault')
    expect_identical('assault', unique(chars3$intent))

  # test mechanism argument ----
    # check that that * gives all intent & mechanism by sampling some commonly of interest
    chars4 <- chars_injury_matrix_count(ph.data = charsdt, mechanism = '*')
    expect_true(sum(c('overexertion', 'firearm', 'fall', 'drowning', 'Any mechanism') %in% unique(chars4$mechanism)) == 5)

    # confirm 'none' collapses the mechanism
    chars5 <- chars_injury_matrix_count(ph.data = charsdt, mechanism = 'none')
    expect_identical('Any mechanism', unique(chars5$mechanism))

    # confirm can select mechanism of interest
    chars6 <- chars_injury_matrix_count(ph.data = charsdt, mechanism = 'firearm')
    expect_identical('firearm', unique(chars6$mechanism))

  # test group_by argument ----
    chars7 <- (chars_injury_matrix_count(ph.data = charsdt, mechanism = 'none', intent = 'none', group_by = 'race4'))
    expect_identical(sort(as.character(chars7$race4)),
                     c("AIAN", "Asian", "Black", "Hispanic", "Multiple", "NHPI", "White"))

  # test def argument ----
    chars8 <- (chars_injury_matrix_count(ph.data = charsdt, mechanism = 'none', intent = 'none', def = 'narrow'))
    chars9 <- (chars_injury_matrix_count(ph.data = charsdt, mechanism = 'none', intent = 'none', def = 'broad'))
    expect_gt(chars9$hospitalizations, chars8$hospitalizations)

  # test primary_ecode argument ----
    # when TRUE, total of any intent & any mechanism should be same as total of all specific mechanisms and causes
    chars10 <- chars_injury_matrix_count(ph.data = charsdt, mechanism = '*', intent = '*', def = 'narrow', primary_ecode = T)
    chars10 <- chars10[mechanism!='motor_vehicle_traffic']
    expect_equal(sum(chars10[intent == 'Any intent' & mechanism == 'Any mechanism']$hospitalizations),
                 sum(chars10[intent != 'Any intent' & mechanism != 'Any mechanism']$hospitalizations) )

    # when FALSE, total of any intent & any mechanism should be LESS THAN total of all specific mechanisms and causes
    chars11 <- chars_injury_matrix_count(ph.data = charsdt, mechanism = '*', intent = '*', def = 'narrow', primary_ecode = F)
    chars11 <- chars11[mechanism!='motor_vehicle_traffic']
    expect_lt(sum(chars11[intent == 'Any intent' & mechanism == 'Any mechanism']$hospitalizations),
              sum(chars11[intent != 'Any intent' & mechanism != 'Any mechanism']$hospitalizations) )
  })

  test_that("Function gives errors as appropriate...", {
    # should error when no search strings given
    expect_error(chars_injury_matrix_count(ph.data = charsdt, intent = NULL, kingco = T))
    expect_error(chars_injury_matrix_count(ph.data = charsdt, mechanism = NULL, kingco = T))

    # should error when mis-specify the name of ph.data
    expect_error(chars_injury_matrix_count(ph.data = 'charsdt', kingco = F))
    expect_error(chars_injury_matrix_count(ph.data = NULL, kingco = F))
    expect_error(chars_injury_matrix_count(ph.data = charsdt2, kingco = F))

    # should error when have illogical group_by values missing chi_geo_kc
    expect_error(chars_injury_matrix_count(ph.data = charsdt, group_by = c('wastate', 'blah', 'age6')))

    # should error because 'blah' is not an intent or mechanism
    expect_error(chars_injury_matrix_count(ph.data = charsdt, intent = 'blah', kingco = F))
    expect_error(chars_injury_matrix_count(ph.data = charsdt, mechanism = 'blah', kingco = F))

    # should error when mis-specify the def argument
    expect_error(chars_injury_matrix_count(ph.data = charsdt, kingco = F, def = 'narrows'))

    # should error when primary_ecode is not T|F
    expect_error(chars_injury_matrix_count(ph.data = charsdt, primary_ecode = "F"))
    expect_error(chars_injury_matrix_count(ph.data = charsdt, primary_ecode = 1))

    # should error when kingco is not T|F
    expect_error(chars_injury_matrix_count(ph.data = charsdt, kingco = "F"))
    expect_error(chars_injury_matrix_count(ph.data = charsdt, kingco = 1))

  })

