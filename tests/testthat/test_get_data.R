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
  t8 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = TRUE, version = 'vdev')
  t9 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = FALSE, version = 'vdev')
  expect_equal(t8, t9)

})

