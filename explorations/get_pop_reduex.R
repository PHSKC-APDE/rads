library('rads')
library('glue')
library('data.table')
library('DBI')
a1 = get_population(years = c(2000, 2010, 2022),
                   ages = 10:20,
                   genders = 'f',
                   races = c('asian', 'black'),
                   race_type = 'race_eth',
                   geo_type = 'county',
                   round = FALSE)

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
