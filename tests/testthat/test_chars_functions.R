library('data.table')
library('testthat')

# Check chars_icd_ccs() ----
  ccstable <- chars_icd_ccs()

  test_that("Check structure of returned table", {
    expect_identical(ccstable, chars_icd_ccs(ref_type = 'all'))

    expect_true(inherits(ccstable, 'data.table')) # confirm data.table
    expect_equal(sort(names(ccstable)), sort(c('broad', 'detailed', 'icdcm', 'icdcm_code', 'icdcm_version')))
    expect_gt(nrow(ccstable), 80000) # realistically over 100K, so checking for gross errors
    expect_gt(nrow(ccstable), nrow(chars_icd_ccs(icdcm_version = 9))) # should have many more rows for ICD-10-CM

    expect_equal(chars_icd_ccs(), chars_icd_ccs(ref_type = 'all'))
    expect_true(inherits(chars_icd_ccs(ref_type = 'icdcm'), 'data.table'))
    expect_equal(sort(names(chars_icd_ccs(ref_type = 'icdcm'))), c('icdcm', 'icdcm_code', 'icdcm_version'))

    expect_true(inherits(chars_icd_ccs(ref_type = 'broad'), 'data.table'))
    expect_equal(names(chars_icd_ccs(ref_type = 'broad')), c('broad', 'icdcm_version'))

    expect_true(inherits(chars_icd_ccs(ref_type = 'detailed'), 'data.table'))
    expect_equal(names(chars_icd_ccs(ref_type = 'detailed')), c('detailed', 'icdcm_version'))

    expect_error(chars_icd_ccs(ref_type = 'blah')) # this isn't a legit ref_type
    expect_error(chars_icd_ccs(mykey = NULL)) # need to use a legit keyring:: service for HHSAW
    expect_error(chars_icd_ccs(icdcm_version = 8)) # Only ICD-CM-9 & ICD-CM-10 are legit

  })

# Check chars_icd_ccs_count() ----
  charsdata <- get_data_chars(year = 2019, cols = c("seq_no", "diag1", "chi_sex"), kingco = T)

  test_that("Function returns expected rows, columns, and values...", {
    # test icd argument ----
      icd.result <- chars_icd_ccs_count(ph.data = charsdata,
                                        icdcm_version = 10,
                                        broad = NULL,
                                        detailed = NULL,
                                        icdcm = '^kidney transplant',
                                        icdcol = 'diag1',
                                        group_by = NULL,
                                        kingco = F,
                                        mykey = 'hhsaw')
      expect_equal(nrow(icd.result), 4)
      expect_equal(sort(names(icd.result)), sort(c('icdcm_desc', 'hospitalizations')))
      expect_equal(sum(icd.result$hospitalizations), nrow(charsdata[grepl("T8611|T8612|T8613|Z940", x = diag1)]))

    # test detailed argument ----
      detailed.result <- chars_icd_ccs_count(ph.data = charsdata,
                                             detailed = 'Cystic fibrosis',
                                             kingco = F)
      expect_equal(nrow(detailed.result), 1)
      expect_equal(sort(names(detailed.result)), sort(c('detailed_desc', 'hospitalizations')))
      expect_equal(sum(detailed.result$hospitalizations), 63) # confirmed vs CHAT

    # test broad argument ----
      broad.result <- chars_icd_ccs_count(ph.data = charsdata,
                                          broad = 'Diseases of the blood',
                                          kingco = F)
      expect_equal(nrow(broad.result), 1)
      expect_equal(sort(names(broad.result)), sort(c('broad_desc', 'hospitalizations')))

    # test group_by argument ----
      group_by.result <- chars_icd_ccs_count(ph.data = charsdata[chi_sex %in% c("Male", "Female")],
                                             detailed = 'chronic kidney disease',
                                             kingco = F,
                                             group_by = 'chi_sex')
      expect_equal(nrow(group_by.result), 2)
      expect_equal(sort(names(group_by.result)), sort(c('chi_sex', 'detailed_desc', 'hospitalizations')))
      expect_equal(sum(group_by.result$hospitalizations), 82) # confirmed vs CHAT

    # test valid combination of search terms (and confirm search is case insensitive) ----
      multi.result <- chars_icd_ccs_count(ph.data = charsdata,
                                          icdcm = 'polyp',
                                          broad = 'neo',
                                          detailed = 'benign',
                                          kingco = F)
      expect_equal(nrow(multi.result), 1)
      expect_equal(sort(names(multi.result)),
                   sort(c('icdcm_desc', 'broad_desc', 'detailed_desc', 'hospitalizations')))

    # test that works for ICD9 (pre 2016) ----
      icd9data <- get_data_chars(year = 2015, cols = c('seq_no', 'diag1', 'chi_geo_kc'))
      icd9counts <- chars_icd_ccs_count(ph.data = icd9data,
                                        icdcm_version = 9,
                                        broad = 'neo',
                                        kingco = T)
      expect_equal(nrow(icd9counts), 2)
      expect_equal(sort(names(icd9counts)),
                   sort(c('broad_desc', 'hospitalizations')))
  })

  test_that("Function gives errors as appropriate...", {
    # should error when no search strings given
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd = NULL, kingco = T))

    # should error when mis-specify the name of ph.data
    expect_error(chars_icd_ccs_count(ph.data = 'charsdata', icd = '^Kidney transplant', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = NULL, icd = '^Kidney transplant', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata2, icd = '^Kidney transplant', kingco = F))

    # should error because missing chi_geo_kc
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd = '^Kidney transplant', kingco = T))

    # should error because 'blah' is not a medical diagnosis!
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd = 'blah', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, broad = 'blah', kingco = F))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, detailed = 'blah', kingco = F))

    # should error when mis-specify the column containing icdcm data
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd = '^Kidney transplant', kingco = F, icdcol = NULL))
    expect_error(chars_icd_ccs_count(ph.data = charsdata, icd = '^Kidney transplant', kingco = F, icdcol = 'mycolumn'))

    # should error due to group_by because 'blah' is not a column in the dataset
    expect_error(chars_icd_ccs_count(ph.data = charsdata, broad = 'Chronic kidney disease', kingco = F, group_by = 'blah'))

    # should error message when filter out all values at more granular level
    expect_error(chars_icd_ccs_count(ph.data = charsdata,
                                     icdcm = 'polyp',
                                     broad = 'neo',
                                     detailed = 'mangoes',
                                     kingco = F))

    # should give warning when pass an icdcol that has non-standard values
    expect_warning(chars_icd_ccs_count(ph.data = charsdata, icdcm = '^Kidney transplant', kingco = F, icdcol = 'chi_sex'))
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
    chars10 <- chars10[mechanism!='motor_vehicle_traffic'] # remove motor_vehcicle_traffic b/c created by RADS based on other vars
    expect_equal(sum(chars10[intent == 'Any intent' & mechanism == 'Any mechanism']$hospitalizations),
                 sum(chars10[intent != 'Any intent' & mechanism != 'Any mechanism']$hospitalizations) )
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

    # should error when primary_ecode == F
    expect_error(chars_injury_matrix_count(ph.data = charsdt, mechanism = '*', intent = '*', def = 'narrow', primary_ecode = F))

  })

