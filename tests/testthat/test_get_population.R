library('testthat')
# all comparison values except for lgd are from CHAT: https://secureaccess.wa.gov/doh/chat/Entry.mvc
# lgd comparison is from SQL data base because legislative district population not given in CHAT

test_that('get_population',{

  expect_error(get_population(years = c(1999, 2009, 2019))) # should error when < 2000

  expect_error(get_population(years = c(2010, 2012), geo_type = "lgd")) # should error when < 2011 & geo_type = lgd

  expect_equal(2190200, get_population(years = 2018)$pop)  # KC 2018

  expect_equal(get_population(years = 2018, geo_type = "kc")$pop, get_population(years = 2018)$pop)  # KC 2018

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "blk", kingco = F))), nrow(get_population(geo_type = "blk", kingco = T))) # confirm kingco=T works for blk

  expect_equal(1104, get_population(years = 2018, geo_type = "blkgrp")[geo_id == "530330017022"]$pop) # 2018 block group == 530330017022

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "blkgrp", kingco = F))), nrow(get_population(geo_type = "blkgrp", kingco = T))) # confirm kingco=T works for blkgrp

  expect_equal(2190200, get_population(years = c(2018), geo_type = 'county')[grepl("King", geo_id, ignore.case = T)]$pop) # KC 2018

  expect_equal(254500, get_population(years = c(2018), geo_type = 'county')[grepl("Yakima", geo_id, ignore.case = T)]$pop) # KC 2018

  expect_equal(19979, get_population(years = 2018, geo_type = c("hra"), group_by = c("geo_id"))[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA

  expect_equal(19979, get_population(years = 2018, geo_type = c("hra"))[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA

  expect_equal(179788, get_population(years = 2018, geo_type = "lgd")[geo_id_code == 53043]$pop) # 2018 43rd Legislative District

  expect_equal(762643, get_population(years = 2018, geo_type = "region")[geo_id == "South"]$pop) # 2018 South Region (defined by block/HRA, not zip)

  expect_equal(731233, get_population(years = 2018, geo_type = "scd")[geo_id == "Seattle School District"]$pop) # 2018 Seattle School District

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "scd", kingco = F))), nrow(get_population(geo_type = "scd", kingco = T))) # confirm kingco=T works for scd

  expect_equal(730920, get_population(years = 2018, geo_type = "seattle")$pop) # 2018 Seattle (defined by block/HRA, not zip)

  expect_equal(1965, suppressWarnings(get_population(years = 2018, geo_type = "tract"))[geo_id == "53033032703"]$pop) # 2018 Tract == 53033032703

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "tract", kingco = F))), nrow(get_population(geo_type = "tract", kingco = T))) # confirm kingco=T works for tract

  expect_equal(35393, get_population(years = 2019, geo_type = c("zip"), group_by = c("geo_id"))[geo_id=="98001"]$pop) # 2019 zip == 98001

  expect_gt( nrow(get_population(geo_type = "zip", kingco = F)), nrow(get_population(geo_type = "zip", kingco = T))) # confirm kingco=T works for zip

  expect_equal(2153700, sum(get_population(years = 2017, genders = c("female", "male"))$pop) ) # 2017 KC (by summing both genders)

  expect_equal(1077304, get_population(years = 2017, genders = "female")$pop ) # KC females 2017

  expect_equal(1076396, get_population(years = 2017, genders = "male")$pop ) # KC males 2017

  expect_equal(2153700, sum(get_population(years = 2017, race_type = "race_eth", group_by = "race_eth")$pop) ) # 2017 KC (by summing all race_eth estimates)

  expect_equal(get_population(race_type = "race_eth", races = c("hispanic"))[]$pop,
               get_population(race_type = "race", races = c("hispanic"), group_by = "race")[]$pop) # check Hispanic ethnicity == Hispanic race when sole race/eth selected, independent of group_by

  expect_equal(get_population(race_type = "race", races = c("hispanic"))[]$pop,
               get_population(race_type = "race_eth", races = c("hispanic"), group_by = "race_eth")[]$pop) # check Hispanic ethnicity == Hispanic race when sole race/eth selected, independent of group_by

  expect_equal(222934, get_population(years = 2018, race_type = "race", races = c("hispanic"))[]$pop) # KC Hispanic pop 2018

  expect_equal(217181, get_population(years = 2017, race_type = "race_eth", race = "hispanic")$pop ) # 2017 Hispanic as race

  expect_equal(1305627, get_population(years = 2017, race_type = "race_eth", race = "white")$pop ) # 2017 White-NH

  expect_gt( get_population(race_type = "race", races = c("hispanic", "white"), group_by = "race")[race == "White"]$pop,
             get_population(race_type = "race_eth", races = c("hispanic", "white"), group_by = "race_eth")[race_eth == "White"]$pop) # White alone or in combination should be >> White-NH

  expect_equal(2353, get_population(years = 2017, race_type = "race_eth", race = "hispanic", genders = "female", ages = c(0))$pop ) # 2017 Hispanic as race, female, age zero

  expect_equal(269, get_population(years = 2017, race_type = "race_eth", race = "white", ages = c(100))$pop ) # 2017 White-NH, age 100+ ... check top coding 100+

  expect_equal("65-100", get_population(years = 2019, ages = c(65:100))[]$age ) # ensure that summary age is properly formatted

  expect_equal(17, nrow(get_population(geo_type = "lgd", years = 2020))) # ensure kingco = T (default) subsets to KC districts only

})
