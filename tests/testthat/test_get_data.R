library('testthat')
library('data.table')

# Birth ----
test_that('Load data birth', {
  # check that NA loads all cols, with lowercase names
  birth1 <- get_data_birth()
  expect_equal(names(birth1), tolower(names(birth1)))

  # check that column & year selection works properly
  birth2 <- get_data_birth(cols = c('chi_geo_kc', 'chi_year'), year = 2017:2019)
  birth2.5 <- get_data_birth(cols = c('chi_year'), year = 2017:2019)
  expect_identical(names(birth2), c('chi_geo_kc', 'chi_year'))
  expect_identical(sort(unique(birth2$chi_year)), 2017:2019)
  expect_error(get_data_birth(year = '2019', cols = 'chi_geo_kc', kingco = T))
  expect_error(get_data_birth(year = 2019.4, cols = 'chi_geo_kc', kingco = T))
  expect_error(get_data_birth(year = 2019, cols = 12, kingco = T))

  # check that kingco works as expected
  birth3 <- get_data_birth(year = 2019, cols = 'chi_geo_kc', kingco = T)
  birth4 <- get_data_birth(year = 2019, cols = 'chi_geo_kc', kingco = F)
  expect_gt(nrow(birth4), nrow(birth3)) # More people in WA than just KC!

  expect_error(get_data_birth(year = 2019, cols = 'chi_geo_kc', kingco = 'zip'))
  expect_error(get_data_birth(year = 2019, cols = 'chi_geo_kc', kingco = NA))
  expect_error(get_data_birth(year = 2019, cols = 'chi_geo_kc', kingco = NULL))

})

# BRFSS ----
test_that('Load data brfss', {
  # specify vars that do not need MI and for multiple years
    brfss0 <- get_data_brfss(cols = c('chi_sex'), year = 2019:2023)
    expect_true(inherits(brfss0, 'dtsurvey'))
    expect_true(inherits(brfss0, 'data.table'))
    expect_equal(sort(c('chi_year', 'chi_sex', 'finalwt1', 'x_ststr', 'default_wt', '_id')),  sort(names(brfss0)))
    expect_identical(2019:2023, as.integer(unique(brfss0$chi_year)))

  # specify vars that do not need MI and a single year
    brfss1 <- get_data_brfss(cols = c('chi_sex'), year = 2022)
    expect_true(inherits(brfss1, 'dtsurvey'))
    expect_true(inherits(brfss1, 'data.table'))
    expect_identical(1L, uniqueN(brfss1$chi_year))

  # specify a var that needs MI and do not specify year
    brfss2 <- get_data_brfss(cols = c('chi_sex', 'hra20_name'))
    brfss2_names <- sort(names(brfss2$imputations[[1]]))
    brfss2_names <- brfss2_names[!grepl('hra20_id_[0-9]', brfss2_names)]
    expect_true(inherits(brfss2, 'imputationList'))
    expect_true(inherits(brfss2$imputations[[1]], 'dtsurvey'))
    expect_true(inherits(brfss2$imputations[[1]], 'data.table'))
    expect_identical(1L, uniqueN(brfss2$imputations[[1]]$chi_year))
    expect_identical(sort(c('chi_year', 'chi_sex', 'finalwt1', 'x_ststr', 'default_wt', '_id', 'hra20_name')), brfss2_names)

  # do not specify a variable and specify multiple years
    brfss3 <- get_data_brfss(cols = NULL, year = 2019:2023)
    expect_true(inherits(brfss3, 'imputationList'))
    expect_true(inherits(brfss3$imputations[[1]], 'dtsurvey'))
    expect_true(inherits(brfss3$imputations[[1]], 'data.table'))
    expect_gt(uniqueN(names(brfss3$imputations[[1]])), 200)
    expect_identical(2019:2023, as.integer(unique(brfss3$imputations[[1]]$chi_year)))

  # check that error messages work appropriately
    expect_error(get_data_brfss(cols = NA), "columns are not available")
    expect_error(get_data_brfss(cols = 'arbustus'), "columns are not available")
    expect_error(get_data_brfss(year = '2020'), "must specify a vector of integers")
    expect_error(get_data_brfss(year = 2020.00001), "must specify a vector of integers")
    expect_error(get_data_brfss(year = 1984), "years are not available in the dataset")

  # confirm wt_method argument generates distinct default_wt values
    ss = get_data_brfss(cols = 'chi_year', year = c(2019, 2023), wt_method = 'simple')
    oo = get_data_brfss(cols = 'chi_year', year = c(2019, 2023), wt_method = 'obs')
    pp = get_data_brfss(cols = 'chi_year', year = c(2019, 2023), wt_method = 'pop')

    expect_equal( sum(ss$finalwt1) / sum(ss$default_wt), 2) # simple should just divide by 2
    expect_equal( round(sum(oo[chi_year == 2019]$default_wt) / sum(oo[chi_year == 2019]$finalwt1), 5),
                  round(nrow(oo[chi_year == 2019]) / nrow(oo), 5)) # proportionate to row counts
    expect_equal( round(sum(pp[chi_year == 2019]$default_wt) / sum(pp[chi_year == 2019]$finalwt1), 5),
                  round(sum(pp[chi_year == 2019]$finalwt1) / sum(pp$finalwt1), 5)) # proportionate to population

})

# Death ----
test_that('Load data death', {
  # check that NA loads all cols, with lowercase names
  death1 <- get_data_death()
  expect_equal(names(death1), tolower(names(death1)))

  # check that column & year selection works properly
  death2 <- get_data_death(cols = c('chi_geo_kc', 'chi_year'), year = 2017:2019)
  death2.5 <- get_data_death(cols = c('chi_year'), year = 2017:2019)
  expect_identical(sort(names(death2)), c('chi_geo_kc', 'chi_year'))
  expect_identical(sort(unique(death2$chi_year)), 2017:2019)

  expect_error(get_data_death(year = '2019', cols = 'chi_geo_kc', kingco = T))
  expect_error(get_data_death(year = 2019.4, cols = 'chi_geo_kc', kingco = T))
  expect_error(get_data_death(year = 2019, cols = 12, kingco = T))

  # check that kingco works as expected
  death3 <- get_data_death(year = 2019, cols = 'chi_geo_kc', kingco = T)
  death4 <- get_data_death(year = 2019, cols = 'chi_geo_kc', kingco = F)
  expect_gt(nrow(death4), nrow(death3)) # More people in WA than just KC!

  expect_error(get_data_death(year = 2019, cols = 'chi_geo_kc', kingco = 'zip'))
  expect_error(get_data_death(year = 2019, cols = 'chi_geo_kc', kingco = NA))
  expect_error(get_data_death(year = 2019, cols = 'chi_geo_kc', kingco = NULL))

  # check topcoding
  death5 <- get_data_death(year = 2020, cols = 'chi_age', kingco = T, topcode = T)
  death6 <- get_data_death(year = 2020, cols = 'chi_age', kingco = T, topcode = F)
  expect_equal(nrow(death6), nrow(death5))
  expect_gt(max(death6[!is.na(chi_age)]$chi_age), max(death5[!is.na(chi_age)]$chi_age))
  expect_equal(max(death5[!is.na(chi_age)]$chi_age), 100)

  expect_error(get_data_death(year = 2019, cols = 'chi_age', topcode = 'blah'))
  expect_error(get_data_death(year = 2019, cols = 'chi_age', topcode = NA))
  expect_error(get_data_death(year = 2019, cols = 'chi_age', topcode = NULL))

})


# CHARS ----
test_that('Load data chars', {
  # check that NA loads all cols, with lowercase names, including CHI vars made on the fly
  chars1 <- get_data_chars()
  expect_equal(names(chars1), tolower(names(chars1)))
  expect_true('race3' %in% names(chars1))
  expect_true('race3_hispanic' %in% names(chars1))
  expect_gt(ncol(chars1), 125)

  # check that column & year selection works properly
  chars2 <- get_data_chars(cols = c('chi_geo_kc', 'chi_year', 'race4', 'seq_no'), year = 2019:2021)
  expect_identical(names(chars2), c('chi_geo_kc', 'chi_year', 'race4', 'seq_no'))
  expect_identical(sort(unique(chars2$chi_year)), 2019:2021)

  # check that kingco works as expected
  chars3 <- get_data_chars(year = 2020, cols = 'chi_geo_kc', kingco = T)
  chars4 <- get_data_chars(year = 2020, cols = 'chi_geo_kc', kingco = F)
  chars5 <- get_data_chars(year = 2020, cols = 'chi_geo_kc', kingco = 'zip')
  expect_gt(nrow(chars4), nrow(chars3)) # More people in WA than just KC!
  expect_true(nrow(chars3) != nrow(chars5))

  # check that wastate works as expected
  chars6 <- get_data_chars(year = 2020, cols = 'wastate', kingco = F, wastate = T)
  chars7 <- get_data_chars(year = 2020, cols = 'wastate', kingco = F, wastate = F)
  expect_gt(nrow(chars7), nrow(chars6))

  # check that inpatient works as expected
  chars8 <- get_data_chars(year = 2020, cols = c('chi_geo_kc', 'flag_inpatient'), kingco = T, inpatient = T)
  chars9 <- get_data_chars(year = 2020, cols = c('chi_geo_kc', 'flag_inpatient'), kingco = T, inpatient = F)
  expect_gt(nrow(chars9), nrow(chars8))
  expect_true(isTRUE(unique(chars8$flag_inpatient)))
  expect_identical(sort(unique(chars9$flag_inpatient)),  c(F, T))

  # check that deaths works as expected
  chars10 <- get_data_chars(year = 2020, cols = c('chi_geo_kc', 'flag_discharged_deceased'), kingco = T, deaths = T) # includes deaths
  chars11 <- get_data_chars(year = 2020, cols = c('chi_geo_kc', 'flag_discharged_deceased'), kingco = T, deaths = F) # no deaths
  expect_gt(nrow(chars10), nrow(chars11))
  expect_true(isFALSE(unique(chars11$flag_discharged_deceased)))
  expect_identical(sort(unique(chars10$flag_discharged_deceased)),  c(F, T))

  # check topcoding
  chars12 <- get_data_chars(year = 2020, cols = 'chi_age', kingco = T, topcode = T)
  chars13 <- get_data_chars(year = 2020, cols = 'chi_age', kingco = T, topcode = F)
  expect_equal(nrow(chars13), nrow(chars12))
  expect_gt(max(chars13[!is.na(chi_age)]$chi_age), max(chars12[!is.na(chi_age)]$chi_age))
  expect_equal(max(chars12[!is.na(chi_age)]$chi_age), 100)

})


# HYS ----
test_that('Load data hys', {

  # load data from ar only
  t1 = get_data_hys('chi_year', year = 2021, ar = TRUE)
  t2 = get_data_hys('chi_year', year = 2021, ar = FALSE) # should overwrite ar option
  expect_equal(t1,t2)

  expect_equal(t1[, unique(chi_year)], 2021)

  # mixed
  t3 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = TRUE)
  t4 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = FALSE)
  expect_equal(t3, t4)

  # staged only
  expect_warning(t5 <- get_data_hys(year = 2021, ar = FALSE), 'Requested staged data only')
  expect_warning(t6 <- get_data_hys(cols = 'year', year = 2021, ar = FALSE), 'Requested staged data only')

  # ar = TRUE, by outside of KC
  expect_warning(t7 <- get_data_hys(ar = TRUE,kingco = F), 'outside of KC')

  # from an alternate version
  t8 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = TRUE, version = 'cifstest')
  t9 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = FALSE, version = 'cifstest')
  expect_equal(t8, t9)

})

