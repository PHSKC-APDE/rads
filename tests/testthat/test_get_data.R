library('testthat')

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
  expect_warning(t7 <- get_data_hys(ar = TRUE), 'outside of KC')

  # from an alternate version
  t8 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = TRUE, version = 'vdev')
  t9 = get_data_hys(c('chi_year', 'year'), year = 2021, ar = FALSE, version = 'vdev')
  expect_equal(t8, t9)

})
