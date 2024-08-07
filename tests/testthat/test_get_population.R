library('testthat')
# all comparison values except for lgd are from CHAT: https://secureaccess.wa.gov/doh/chat/Entry.mvc
# lgd comparison is from SQL data base because legislative district population not given in CHAT

# Test some queries. Mostly for manual inspection, but it'll be good to run these in case they break all of a sudden
test_that('get_population queries',{

  q1.1 = get_population(ages = 10:20, genders = 'm', races = c('aian', 'hispanic'),
                        race_type = 'race_eth', group_by = 'race_eth', census_vintage = 2020, geo_vintage = 2020, return_query = T)
  expect_true(inherits(q1.1, 'SQL'))

  q1.2 = get_population(ages = 10:20, genders = 'm', races = c('aian', 'hispanic'), race_type = 'race', group_by = 'race', census_vintage = 2020, geo_vintage = 2020, return_query = T)
  expect_true(inherits(q1.2, 'list'))

  q1.3 = get_population(ages = 10:20, genders = 'm', races = c('aian', 'hispanic'), census_vintage = 2020, geo_vintage = 2020, race_type = 'race_aic', group_by = 'race_aic', return_query = T)
  expect_true(inherits(q1.3, 'list'))

  q2.1 = get_population(ages = 10:20, genders = 'm', race_type = 'race_eth', census_vintage = 2020, geo_vintage = 2020, group_by = 'race_eth', return_query = T)
  expect_true(inherits(q2.1, 'SQL'))

  q2.2 = get_population(ages = 10:20, genders = 'm', race_type = 'race', census_vintage = 2020, geo_vintage = 2020, group_by = 'race', return_query = T)
  expect_true(inherits(q2.2, 'list'))

  q2.3 = get_population(ages = 10:20, genders = 'm', race_type = 'race_aic', census_vintage = 2020, geo_vintage = 2020, group_by = 'race_aic', return_query = T)
  expect_true(inherits(q2.3, 'list'))



})


test_that('get_population',{

  expect_error(get_population(years = c(1999, 2009, 2019), geo_vintage = 2020)) # should error when < 2000

  # expect_error(get_population(years = c(2010, 2012), geo_type = "lgd", geo_vintage = 2010, census_vintage = 2010)) # should error when < 2011 & geo_type = lgd

  # expect_equal(2190200, get_population(years = 2018, geo_vintage = 2010, census_vintage = 2010)$pop)  # KC 2018

  expect_equal(get_population(years = 2018, geo_type = "kc", census_vintage = 2020, geo_vintage = 2020)$pop, get_population(years = 2018, census_vintage = 2020, geo_vintage = 2020)$pop)  # KC 2018

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "zip", kingco = F, census_vintage = 2020, geo_vintage = 2020))), nrow(get_population(geo_type = "zip", kingco = T, census_vintage = 2020, geo_vintage = 2020))) # confirm kingco=T works

  # expect_equal(1104, get_population(years = 2018, geo_type = "blkgrp", geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id == "530330017022"]$pop) # 2018 block group == 530330017022

  # expect_gt( nrow(suppressWarnings(get_population(geo_type = "blkgrp", kingco = F))), nrow(get_population(geo_type = "blkgrp", kingco = T))) # confirm kingco=T works for blkgrp

  # expect_equal(2190200, get_population(years = c(2018), geo_type = 'county', geo_vintage = 2010, census_vintage = 2010, round = T)[grepl("King", geo_id, ignore.case = T)]$pop) # KC 2018

  # expect_equal(254500, get_population(years = c(2018), geo_type = 'county', geo_vintage = 2010, census_vintage = 2010, round = T)[grepl("Yakima", geo_id, ignore.case = T)]$pop) # KC 2018
  #
  # expect_equal(19979, get_population(years = 2018, geo_type = c("hra"), group_by = c("geo_id"), geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA
  #
  # expect_equal(19979, get_population(years = 2018, geo_type = c("hra"), geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id == "North Highline"]$pop ) # 2018 North Highline HRA
  #
  # expect_equal(179788, get_population(years = 2018, geo_type = "lgd", geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id_code == 53043]$pop) # 2018 43rd Legislative District
  #
  # expect_equal(762643, get_population(years = 2018, geo_type = "region", geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id == "South"]$pop) # 2018 South Region (defined by block/HRA, not zip)
  #
  # expect_equal(731233, get_population(years = 2018, geo_type = "scd", geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id == "Seattle School District"]$pop) # 2018 Seattle School District

  expect_gt( nrow(suppressWarnings(get_population(geo_type = "scd", kingco = F, census_vintage = 2020, geo_vintage = 2020))), nrow(get_population(geo_type = "scd", kingco = T, census_vintage = 2020, geo_vintage = 2020))) # confirm kingco=T works for scd

  # expect_equal(730920, get_population(years = 2018, geo_type = "seattle", geo_vintage = 2010, census_vintage = 2010, round = T)$pop) # 2018 Seattle (defined by block/HRA, not zip)
  #
  # expect_equal(1965, suppressWarnings(get_population(years = 2018, geo_type = "tract", geo_vintage = 2010, census_vintage = 2010, round = T))[geo_id == "53033032703"]$pop) # 2018 Tract == 53033032703

  # expect_gt( nrow(suppressWarnings(get_population(geo_type = "tract", kingco = F))), nrow(get_population(geo_type = "tract", kingco = T))) # confirm kingco=T works for tract

  # expect_equal(35393, get_population(years = 2019, geo_type = c("zip"), group_by = c("geo_id"), geo_vintage = 2010, census_vintage = 2010, round = T)[geo_id=="98001"]$pop) # 2019 zip == 98001

  expect_gt( nrow(get_population(geo_type = "zip", kingco = F, census_vintage = 2020, geo_vintage = 2020)), nrow(get_population(geo_type = "zip", kingco = T, census_vintage = 2020, geo_vintage = 2020))) # confirm kingco=T works for zip

  # expect_equal(2153700, sum(get_population(years = 2017, genders = c("female", "male"), geo_vintage = 2010, census_vintage = 2010, round = T)$pop) ) # 2017 KC (by summing both genders)
  # fp = get_population(years = 2017, genders = "female", geo_vintage = 2010, census_vintage = 2010, round = T)
  # expect_equal(1077304, fp$pop ) # KC females 2017
  # expect_equal(fp$gender,'Female')

  # fp2 = get_population(years = 2017, genders = "male", geo_vintage = 2010, census_vintage = 2010, round = T)
  # expect_equal(1076396, fp2$pop ) # KC males 2017
  # expect_equal(fp2$gender, 'Male')

  # expect_equal(2153700, sum(get_population(years = 2017, race_type = "race_eth", group_by = "race_eth", geo_vintage = 2010, census_vintage = 2010, round = T)$pop) ) # 2017 KC (by summing all race_eth estimates)

  expect_equal(get_population(race_type = "race_eth", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020)[]$pop,
               get_population(race_type = "race", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020, group_by = "race")[]$pop) # check Hispanic ethnicity == Hispanic race when sole race/eth selected, independent of group_by

  expect_error(get_population(race_type = "race", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020)[]$pop)

  # expect_equal(222934, get_population(years = 2018, race_type = "race", races = c("hispanic"), group_by = 'race',geo_vintage = 2010, census_vintage = 2010, round = T)[]$pop) # KC Hispanic pop 2018
  #
  # expect_equal(217181, get_population(years = 2017, race_type = "race_eth", race = "hispanic", geo_vintage = 2010, census_vintage = 2010, round = T)$pop ) # 2017 Hispanic as race
  #
  # expect_equal(1305627, get_population(years = 2017, race_type = "race_eth", race = "white", geo_vintage = 2010, census_vintage = 2010, round = T)$pop ) # 2017 White-NH

  expect_gt( get_population(race_type = "race", races = c("hispanic", "white"), group_by = "race", census_vintage = 2020, geo_vintage = 2020)[race == "White"]$pop,
             get_population(race_type = "race_eth", races = c("hispanic", "white"), group_by = "race_eth", census_vintage = 2020, geo_vintage = 2020)[race_eth == "White"]$pop) # White >> White-NH

  # expect_equal(2353, get_population(years = 2017, race_type = "race_eth", race = "hispanic", genders = "female", ages = c(0), geo_vintage = 2010, census_vintage = 2010, round = T)$pop ) # 2017 Hispanic as race, female, age zero
  #
  # expect_equal(269, get_population(years = 2017, race_type = "race_eth", race = "white", ages = c(100), geo_vintage = 2010, census_vintage = 2010, round = T)$pop ) # 2017 White-NH, age 100+ ... check top coding 100+
  #
  # expect_equal("65-100", get_population(years = 2019, ages = c(65:100), geo_vintage = 2010, census_vintage = 2010, round = T)[]$age ) # ensure that summary age is properly formatted
  #
  # expect_equal(17, nrow(get_population(geo_type = "lgd", years = 2020, geo_vintage = 2010, census_vintage = 2010, round = T))) # ensure kingco = T (default) subsets to KC districts only
  #
  # expect_equal(17, nrow(get_population(geo_type = "lgd", years = 2020, geo_vintage = 2020, census_vintage = 2020, round = T))) # ensure kingco = T (default) subsets to KC districts only
  #
  # expect_equal(7656200, get_population(geo_type = "wa", years = 2020,geo_vintage = 2010, census_vintage = 2010, round = T)[]$pop) # Washington State population

  # expectations for race_eth, race, and race_aic
  r1.1 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race_eth', group_by = 'race_eth', geo_vintage = 2020, census_vintage = 2020)
  r1.2 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race', group_by = 'race', geo_vintage = 2020, census_vintage = 2020)
  r1.3 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race_aic', group_by = 'race_aic', geo_vintage = 2020, census_vintage = 2020)
  r1.1 = r1.1[, .(geo_id, year, age, gender, race = race_eth, pop1.1 = pop)]
  r1.2 = r1.2[, .(geo_id, year, age, gender, race = race, pop1.2 = pop)]
  r1.3 = r1.3[, .(geo_id, year, age, gender, race = race_aic, pop1.3 = pop)]

  r1 = merge(r1.1, r1.2, all = T, by = c('geo_id', 'year', 'age', 'gender', 'race'))
  r1 = merge(r1, r1.3, all = T, by = c('geo_id', 'year', 'age', 'gender', 'race'))
  expect_true(r1[race == 'Hispanic', all(all.equal(pop1.1, pop1.2) & all.equal(pop1.3, pop1.2))])
  expect_true(r1[race == 'AIAN', all(pop1.1 <= pop1.2 & pop1.2 <= pop1.3)])

  # make sure hispanic doesn't sneak through
  # This warning expectation will need to be removed when geo_vintage 2020 HRAs and other stuff is available.
  (r2 <- get_population(races = 'aian', group_by = 'race'))
  expect_true(all(r2[, race_eth] == 'AIAN'))

  # Things that shouldn't break
  # warning might need to be removed


  r2 = get_population(geo_type = 'wa',
                      race_type = 'race_eth',
                      group_by = c('ages', 'geo_id'),
                      years = 2016:2020,
                      round = F, geo_vintage = 2020)
  expect_true(all(0:100 %in% r2[,age]))

  r3 <- get_population(geo_type = 'blk', race_type = 'race_eth', races = 'black', group_by = c('ages', 'geo_id'), years = 2018:2020, round = F)
  expect_true(all(!is.na(r3[,age])))

  # new HRA
  r4.1 = get_population(geo_type = 'hra', geo_vintage = 2020, census_vintage = 2020)
  # r4.2 = get_population(geo_type = 'hra', geo_vintage = 2010, census_vintage = 2020)
  # r4.3 = get_population(geo_type = 'hra', geo_vintage = 2010, census_vintage = 2010)
  expect_error(get_population(geo_type = 'hra', geo_vintage = 2020, census_vintage = 2010))
  expect_equal(nrow(r4.1), 61)
  # expect_equal(nrow(r4.2), 48)
  # expect_equal(nrow(r4.3), 48)
  # expect_true(!(all(sort(r4.2[, pop]) - sort(r4.3[, pop]) == 0)))

  # new regions
  # r5.1 = get_population(geo_type = 'region', geo_vintage = 2010, census_vintage = 2020)
  r5.2 = get_population(geo_type = 'region', geo_vintage = 2020, census_vintage = 2020)
  # expect_equal(nrow(r5.1), 4)
  expect_equal(nrow(r5.2), 4)
  expect_true(r5.2[,all(geo_id_code %in% 1:4)])

  r6.1 = get_population(gender = 'f')
  r6.2 = get_population()
  expect_true(r6.1[,pop] < r6.2[,pop])
  expect_equal(r6.1[, gender], 'Female')
  expect_equal(r6.2[, gender], 'Female, Male')

})

test_that('get_population works with passed db connection',{
  r1 = get_population()

  mycon <- DBI::dbConnect(
    odbc::odbc(),
    driver = getOption("rads.odbc_version"),
    server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
    database = "hhs_analytics_workspace",
    uid = keyring::key_list('hhsaw')[["username"]],
    pwd = keyring::key_get('hhsaw', keyring::key_list('hhsaw')[["username"]]),
    Encrypt = "yes",
    TrustServerCertificate = "yes",
    Authentication = "ActiveDirectoryPassword")

  r2 = get_population(mykey = mycon)
  expect_equal(r1,r2)
})

test_that('hispanic option for group by',{
  t1 = get_population(group_by = c('hispanic'), return_query = F)
  t2 = get_population(group_by = c('hispanic', 'race_eth'), return_query = F)
  t3 = get_population(group_by = c('hispanic', 'race_eth'), return_query = F, races = c('white', 'black'))

  expect_equal(t1[hispanic == 'Not Hispanic', pop], t2[hispanic != 'Hispanic', sum(pop)])
  expect_equal(t2[hispanic == 'Hispanic' & race_eth == 'White', pop], t2[hispanic == 'Hispanic' & race_eth == 'White', sum(pop)])
  expect_equal(t1[1, race_eth], 'AIAN, Asian, Black, Multiple race, NHPI, White')

  expect_equal(
    expect_warning(get_population(race_type = 'race_eth', races = 'hispanic', group_by = c('hispanic'))$pop),
    get_population(race_type = 'race_eth', group_by = c('hispanic'))[hispanic == 'Hispanic', pop]
  )
  expect_error(get_population(race_type = 'race_eth', races = c('white', 'hispanic'), group_by = c('hispanic'))[])
})

test_that('subset without grouping',{

  t1 = get_population(races = c("aian", "asian"))
  expect_equal(t1$race_eth, 'AIAN, Asian')
  expect_error(get_population(races = c("All", "asian")))


})

test_that('new dchs stuff',{
  # Just run these to make sure nothing breaks
  r1 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x))
  r2 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x, races = 'NHPI'))
  r3 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x, group_by = 'race', races = 'White', race_type = 'race'))
})

test_that('core geographies',{
  cg = c('blk', 'blkgrp', 'tract', 'county', 'hra', 'kc', 'lgd',
         'region', 'seattle', 'scd' , 'tract', 'wa', 'zip')
  # Just run these to make sure nothing breaks
  r1 = lapply(cg, function(x) get_population(geo_type = x))
  r2 = lapply(cg, function(x) get_population(geo_type = x, races = 'Asian'))
  r3 = lapply(cg, function(x) get_population(geo_type = x, group_by = 'race', races = 'black', race_type = 'race'))

})
