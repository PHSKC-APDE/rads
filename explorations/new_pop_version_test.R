library('rads')
library('testthat')
con = DBI::dbConnect(odbc::odbc(),
                     driver = getOption('rads.odbc_version'),
                     server = keyring::key_list(service = 'azure_server')$username[1],
                     database = 'inthealth_edw',
                     uid = keyring::key_list(mykey)[["username"]],
                     pwd = keyring::key_get(mykey, keyring::key_list(mykey)[["username"]]),
                     Encrypt = 'yes',
                     TrustServerCertificate = 'yes',
                     Authentication = 'ActiveDirectoryPassword')
prefix = 'pop_geo_'
schema = 'stg_reference'


test_that('get_population',{
  
  expect_error(get_population(years = c(1999, 2009, 2019), geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)) # should error when < 2000
  
  # expect_error(get_population(years = c(2010, 2012), geo_type = "lgd", geo_vintage = 2010, census_vintage = 2010)) # should error when < 2011 & geo_type = lgd
  
  expect_equal(get_population(years = 2018, geo_type = "kc", census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)$pop, 
               get_population(years = 2018, census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)$pop)  # KC 2018
  
  expect_gt( nrow(suppressWarnings(get_population(geo_type = "zip", kingco = F, census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con))), 
             nrow(get_population(geo_type = "zip", kingco = T, census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con))) # confirm kingco=T works
  
  
  expect_equal(get_population(race_type = "race_eth", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)$pop,
               get_population(race_type = "race", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020, group_by = "race", schema = schema, table_prefix = prefix, mykey = con)$pop) # check Hispanic ethnicity == Hispanic race when sole race/eth selected, independent of group_by
  
  expect_error(get_population(race_type = "race", races = c("hispanic"), census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)$pop)
  
  
  expect_gt( get_population(race_type = "race", races = c("hispanic", "white"), group_by = "race", census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)[race == "White"]$pop,
             get_population(race_type = "race_eth", races = c("hispanic", "white"), group_by = "race_eth", census_vintage = 2020, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)[race_eth == "White"]$pop) # White >> White-NH
  
  
  expect_equal(17, nrow(get_population(geo_type = "lgd", years = 2020, geo_vintage = 2020, census_vintage = 2020, round = T, schema = schema, table_prefix = prefix, mykey = con))) # ensure kingco = T (default) subsets to KC districts only
  
  # expectations for race_eth, race, and race_aic
  r1.1 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race_eth', group_by = 'race_eth', geo_vintage = 2020, census_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)
  r1.2 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race', group_by = 'race', geo_vintage = 2020, census_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)
  r1.3 = get_population(geo_type = 'county', years = c(2000,2010,2022), ages = c(0:10, 90:100), genders = 'F', races = c('aian', 'hispanic'), race_type = 'race_aic', group_by = 'race_aic', geo_vintage = 2020, census_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)
  r1.1 = r1.1[, .(geo_id, year, age, gender, race = race_eth, pop1.1 = pop)]
  r1.2 = r1.2[, .(geo_id, year, age, gender, race = race, pop1.2 = pop)]
  r1.3 = r1.3[, .(geo_id, year, age, gender, race = race_aic, pop1.3 = pop)]
  
  r1 = merge(r1.1, r1.2, all = T, by = c('geo_id', 'year', 'age', 'gender', 'race'))
  r1 = merge(r1, r1.3, all = T, by = c('geo_id', 'year', 'age', 'gender', 'race'))
  expect_true(r1[race == 'Hispanic', all(all.equal(pop1.1, pop1.2) & all.equal(pop1.3, pop1.2))])
  expect_true(r1[race == 'AIAN', all(pop1.1 <= pop1.2 & pop1.2 <= pop1.3)])
  
  # make sure hispanic doesn't sneak through
  # This warning expectation will need to be removed when geo_vintage 2020 HRAs and other stuff is available.
  (r2 <- get_population(races = 'aian', group_by = 'race', schema = schema, table_prefix = prefix, mykey = con))
  expect_true(all(r2[, race_eth] == 'AIAN'))
  
  # Things that shouldn't break
  # warning might need to be removed
  
  
  r2 = get_population(geo_type = 'wa',
                      race_type = 'race_eth',
                      group_by = c('ages', 'geo_id'),
                      years = 2016:2020,
                      round = F, geo_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)
  expect_true(all(0:100 %in% r2[,age]))
  
  r3 <- get_population(geo_type = 'blk', race_type = 'race_eth', races = 'black', group_by = c('ages', 'geo_id'), years = 2018:2020, round = F, schema = schema, table_prefix = prefix, mykey = con)
  expect_true(all(!is.na(r3[,age])))
  
  # new HRA
  r4.1 = get_population(geo_type = 'hra', geo_vintage = 2020, census_vintage = 2020, schema = schema, table_prefix = prefix, mykey = con)
  #r4.2 = get_population(geo_type = 'hra', geo_vintage = 2010, census_vintage = 2020)
  #r4.3 = get_population(geo_type = 'hra', geo_vintage = 2010, census_vintage = 2010)
  # expect_error(get_population(geo_type = 'hra', geo_vintage = 2020, census_vintage = 2010))
  # expect_equal(nrow(r4.1), 61)
  # expect_equal(nrow(r4.2), 48)
  # expect_equal(nrow(r4.3), 48)
  # expect_true(!(all(sort(r4.2[, pop]) - sort(r4.3[, pop]) == 0)))
  
  # new regions
  # r5.1 = get_population(geo_type = 'region', geo_vintage = 2010, census_vintage = 2020)
  # r5.2 = get_population(geo_type = 'region', geo_vintage = 2020, census_vintage = 2020)
  # expect_equal(nrow(r5.1), 4)
  # expect_equal(nrow(r5.2), 4)
  # expect_true(r5.2[,all(geo_id_code %in% 1:4)])
  
  r6.1 = get_population(gender = 'f', schema = schema, table_prefix = prefix, mykey = con)
  r6.2 = get_population(schema = schema, table_prefix = prefix, mykey = con)
  expect_true(r6.1[,pop] < r6.2[,pop])
  expect_equal(r6.1[, gender], 'Female')
  expect_equal(r6.2[, gender], 'Female, Male')
  
})


test_that('hispanic option for group by',{
  t1 = get_population(group_by = c('hispanic'), return_query = F, schema = schema, table_prefix = prefix, mykey = con)
  t2 = get_population(group_by = c('hispanic', 'race_eth'), return_query = F, schema = schema, table_prefix = prefix, mykey = con)
  t3 = get_population(group_by = c('hispanic', 'race_eth'), return_query = F, races = c('white', 'black'), schema = schema, table_prefix = prefix, mykey = con)
  
  expect_equal(t1[hispanic == 'Not Hispanic', pop], t2[hispanic != 'Hispanic', sum(pop)])
  expect_equal(t2[hispanic == 'Hispanic' & race_eth == 'White', pop], t2[hispanic == 'Hispanic' & race_eth == 'White', sum(pop)])
  expect_equal(t1[1, race_eth], 'AIAN, Asian, Black, Multiple race, NHPI, White')
  
  expect_equal(
    expect_warning(get_population(race_type = 'race_eth', races = 'hispanic', group_by = c('hispanic'), schema = schema, table_prefix = prefix, mykey = con)$pop),
    get_population(race_type = 'race_eth', group_by = c('hispanic'), schema = schema, table_prefix = prefix, mykey = con)[hispanic == 'Hispanic', pop]
  )
  expect_error(get_population(race_type = 'race_eth', races = c('white', 'hispanic'), group_by = c('hispanic'), schema = schema, table_prefix = prefix, mykey = con)[])
})

test_that('subset without grouping',{
  
  t1 = get_population(races = c("aian", "asian"), schema = schema, table_prefix = prefix, mykey = con)
  expect_equal(t1$race_eth, 'AIAN, Asian')
  expect_error(get_population(races = c("All", "asian"), schema = schema, table_prefix = prefix, mykey = con))
  
  
})

test_that('new dchs stuff',{
  # Just run these to make sure nothing breaks
  r1 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x, schema = schema, table_prefix = prefix, mykey = con))
  r2 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x, races = 'NHPI', schema = schema, table_prefix = prefix, mykey = con))
  r3 = lapply(c('ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal'), function(x) get_population(geo_type = x, group_by = 'race', races = 'White', race_type = 'race', schema = schema, table_prefix = prefix, mykey = con))
})

test_that('core geographies',{
  cg = c('blk', 'blkgrp', 'tract', 'county', 'hra', 'kc', 'lgd',
         'region', 'seattle', 'scd' , 'tract', 'wa', 'zip')
  # Just run these to make sure nothing breaks
  r1 = lapply(cg, function(x) get_population(geo_type = x, schema = schema, table_prefix = prefix, mykey = con))
  r2 = lapply(cg, function(x) get_population(geo_type = x, races = 'Asian', schema = schema, table_prefix = prefix, mykey = con))
  r3 = lapply(cg, function(x) get_population(geo_type = x, group_by = 'race', races = 'black', race_type = 'race', schema = schema, table_prefix = prefix, mykey = con))
  
})

# Check geo_ids
valid_geogs = c('tract', 'county', 'hra', 'kc', 'lgd',
                'region', 'seattle', 'scd' , 'tract', 'wa', 'zip',
                'ccl', 'csa', 'inc_uninc', 'puma', 'kccd', 'tribal')

p = sapply(valid_geogs, function(x){
  a = get_population(geo_type = x, schema = schema, table_prefix = prefix, mykey = con)
  b = get_population(geo_type = x)
  
  length(setdiff(a$geo_id,b$geo_id))
  
})
