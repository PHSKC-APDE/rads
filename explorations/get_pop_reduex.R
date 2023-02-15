library('rads')
library('glue')
library('data.table')
library('DBI')
source('explorations/old_get_pop.R')

q1.1 = get_population(years = c(2000, 2010, 2019),
                      ages = 10:20,
                      genders = 'f',
                      races = c('asian', 'black'),
                      race_type = 'race_eth',
                      geo_type = 'county',
                      round = FALSE,
                      return_query = TRUE)
q1.2 = old_get_population(years = c(2000, 2010, 2019),
                          ages = 10:20,
                          genders = 'f',
                          races = c('asian', 'black'),
                          race_type = 'race_eth',
                          geo_type = 'county',
                          round = FALSE,
                          return_query = TRUE)




a1.1 = system.time(r1.1 <- get_population(years = c(2000, 2010, 2019),
                                          ages = 10:20,
                                          genders = 'f',
                                          races = c('asian', 'black'),
                                          race_type = 'race_eth',
                                          geo_type = 'county',
                                          round = FALSE,
                                          return_query = FALSE))

a1.2 = system.time(r1.2 <- old_get_population(years = c(2000, 2010, 2019),
                    ages = 10:20,
                    genders = 'f',
                    races = c('asian', 'black'),
                    race_type = 'race_eth',
                    geo_type = 'county',
                    round = FALSE))

a2 = get_population(years = c(2000, 2010, 2022),
                    genders = 'f',
                    races = c('asian', 'black'),
                    race_type = 'race_eth',
                    geo_type = 'county',
                    group_by = c('years'),
                    round = FALSE)

a3 = get_population(years = c(2000, 2010, 2022),
                    genders = 'f',
                    races = c('asian', 'black'),
                    race_type = 'race_eth',
                    geo_type = 'wa',
                    group_by = c('years'),
                    round = FALSE)
