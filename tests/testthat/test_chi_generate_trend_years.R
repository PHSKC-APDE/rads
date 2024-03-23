test_that("chi_generate_trend_years calculates trends") {
  DT <- chi_generate_trend_years(indicator_key = c("test1", "test2"),span = 3,begin.year = 2009,final.year = 2023)
}
