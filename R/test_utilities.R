test_that('format_years',{

  expect_equal('2010', format_years(2010))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_years(c(2000, 2014:2016, 3000, 3002:4000)))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_years(c(3002:4000, 2000, 2014:2016, 3000)))


})
