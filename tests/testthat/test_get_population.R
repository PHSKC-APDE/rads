library('testthat')
test_that('get_population',{

  expect_equal(2190200, get_population(years = 2018)[]$pop)  # KC 2018

  expect_equal(19979, get_population(years = 2018, geo_type = c("hra"), group_by = c("geo_id"))[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA

  expect_equal(19979, get_population(years = 2018, geo_type = c("hra"))[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA

  expect_equal(2153700, sum(get_population(years = 2017, genders = c("female", "male"))[]$pop) ) # 2017 KC (by summing both genders)

  expect_equal(1077304, get_population(years = 2017, genders = "female")[]$pop ) # KC females 2017

  expect_equal(1076396, get_population(years = 2017, genders = "male")[]$pop ) # KC males 2017

  expect_equal(2153700, sum(get_population(years = 2017, race_type = "race_eth", group_by = "race_eth")[]$pop) ) # 2017 KC (by summing all race_eth estimates)

  expect_equal(217181, get_population(years = 2017, race_type = "race_eth", race = "hispanic")[]$pop ) # 2017 Hispanic as race

  expect_equal(1305627, get_population(years = 2017, race_type = "race_eth", race = "white")[]$pop ) # 2017 White-NH

})


