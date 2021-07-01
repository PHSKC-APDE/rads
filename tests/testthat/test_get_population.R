library('testthat')
test_that('get_population',{

  expect_equal(2190200, get_population(years = 2018)[]$pop)  # KC 2018

  expect_equal(19979, get_population(years = 2018, geo_type = c("hra"), group_by = c("geo_id"))[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA

  expect_equal(2190200, sum(get_population(years = 2018, genders = c("female", "male"))[]$pop) ) # 2018 KC (by summing both genders)

  expect_equal(1095241, get_population(years = 2018, genders = "female")[]$pop ) # KC females 2018

  expect_equal(1094959, get_population(years = 2018, genders = "male")[]$pop ) # KC males 2018

  expect_equal(2190200, sum(get_population(years = 2018, race_type = "race_eth", group_by = "race_eth")[]$pop) ) # 2018 KC (by summing all race_eth estimates)

  expect_equal(217181, get_population(years = 2018, race_type = "race_eth", race = "hispanic")[]$pop ) # 2018 Hispanic as race

  expect_equal(1305627, get_population(years = 2018, race_type = "race_eth", race = "white")[]$pop ) # 2018 White-NH

})


