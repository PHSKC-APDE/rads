library('data.table')
library('testthat')

# Check death_113 ----
  # death_113() create data ----
  # not necessary

  # death_113() create output ----
  # not necessary

  # death_113() tests ----
  test_that("Check that NCHS 113 cause ids are saved to data.table object and have correct structure ...", {
    d113ids <- death_113()
    expect_equal(nrow(d113ids), 114) # Should have 114 rows (113 + alternate version of causeid==90)
    expect_equal(length(unique(d113ids$causeid)), 114)
    expect_equal(length(unique(d113ids$cause.of.death)), 114)
    expect_equal(names(d113ids), c("causeid", "cause.of.death"))
    expect_equal(d113ids, unique(d113ids))
  })


# Check death_113_count ----
  # death_113_count() create data ----
  set.seed(98104)
  # create synthetic line level data
  d113data <- data.table::data.table(
    cod.icd10 = c(rep("A85.2", round(runif(1, 30, 10000), 0)), # code should drop the decimal
                  rep("B51", round(runif(1, 30, 10000), 0)),
                  rep("U071", round(runif(1, 30, 10000), 0)),
                  rep("E44", round(runif(1, 30, 10000), 0)),
                  rep("E62", round(runif(1, 30, 10000), 0)),
                  rep("G00", round(runif(1, 30, 10000), 0)),
                  rep("J10", round(runif(1, 30, 10000), 0)),
                  rep("J15", round(runif(1, 30, 10000), 0)),
                  rep("V874", round(runif(1, 30, 10000), 0)))
  )

  d113data2 <- data.table::copy(d113data)
  set.seed(98104)
  d113data2[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
            1:nrow(d113data2)]
  d113data2[, strata := sample(c("one", "two", "three"), size = nrow(d113data2), replace = T)]

  # create reference table for testing (I know these values are correct because I chose the ICD10 #s)
  COD = data.table(cod = c("Arthropod-borne viral encephalitis",
                           "Malaria",
                           "COVID-19 (U07.1)",
                           "Malnutrition",
                           "Other nutritional deficiencies",
                           "Meningitis",
                           "Influenza",
                           "Pneumonia",
                           "Motor vehicle crash"),
                   id = gsub("\\.", "", sort(unique(d113data$cod.icd10))) )

  # death_113_count() create output ----
  d113res.default <- suppressWarnings(death_113_count(ph.data = d113data,
                                                      causeids = seq(1, 113, 1),
                                                      cause = NULL,
                                                      icdcol = "cod.icd10",
                                                      kingco = FALSE))

  d113res.manual <- copy(d113data)
  d113res.manual[, cod.icd10 := gsub("\\.", "", cod.icd10)]
  d113res.manual <- merge(d113res.manual, COD, by.x = "cod.icd10", by.y = "id", all = TRUE)
  d113res.manual <- d113res.manual[, .(manual.count = .N), cod]

  # death_113_count() tests ----
  test_that("Check for proper triggering of errors ...", {
    expect_error(death_113_count(ph.data = "d113data", kingco = FALSE)) # name of data.table must be unquoted
    expect_warning(death_113_count(ph.data = d113data, icdcol = "cod.icd10", kingco = FALSE)) # warning because of period in A85.2
    expect_error(suppressWarnings(death_113_count(ph.data = d113data, kingco = TRUE))) # Should error because there is no KC data here
    expect_error(death_113_count(ph.data = d113data, causeids = seq(1, 115, 1), kingco = FALSE)) # Should error because highest causeid is 114
    expect_error(death_113_count(ph.data = d113data, causeids = seq(0, 114, 1), kingco = FALSE)) # Should error because lowest causeid is 0
    expect_error(death_113_count(ph.data = d113data, causeids = c(2, 5, "7", 13), kingco = FALSE)) # Should error because of a non-numeric causeid
    expect_error(death_113_count(ph.data = d113data, causeids = c(2, 5, 7.3, 13), kingco = FALSE)) # Should error because of a non-integer causeid
    expect_error(death_113_count(ph.data = d113data, icdcol = cod.icd10, kingco = FALSE)) # Should error because icdcol should be quoted
    expect_error(death_113_count(ph.data = d113data, icdcol = "cod.icd10x", kingco = FALSE)) # Should error because icdcol does not exist
    expect_error(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = "65"))) # Should error because ypll_age is character
    expect_error(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65))) # Should error bc need to specify dob/dod or death_age_col
    expect_error(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65, death_age_col = ageofdeath))) # Should error ageofdeath not quoted
    expect_error(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = 'cod.icd10', kingco = F, group_by = c('stratum')))) # stratum does not exist, should be `strata`
  })

  test_that("Death counts by cause are accurate ...", {
    expect_equal(nrow(d113res.default), nrow(d113res.manual) + 1 ) # 1 extra rows in function (cause.of.death == 'All causes')
    expect_equal(length(intersect(d113res.default$cause.of.death, d113res.manual$cod)), 9) # confirm names of causes of death
    expect_equal(sum(d113res.default[!cause.of.death %in% c('All causes')]$deaths),
                 sum(d113res.manual$manual.count)) # confirm count
    expect_equal(suppressWarnings(sum(death_113_count(ph.data = d113data2, icdcol = 'cod.icd10', kingco = F, group_by = c('strata'))[cause.of.death == 'All causes']$deaths)),
                 d113res.default[cause.of.death == 'All causes']$deaths) # confirm no changes in total deaths when using group_by (all cause)
    expect_equal(suppressWarnings(sum(death_113_count(ph.data = d113data2, icdcol = 'cod.icd10', kingco = F, group_by = c('strata'))[cause.of.death != 'All causes']$deaths)),
                 sum(d113res.default[cause.of.death != 'All causes']$deaths)) # confirm no changes in total deaths when using group_by (cause specific)
  })

  test_that("'cause' argument works correctly ...", {
    expect_equal(
      suppressWarnings(death_113_count(ph.data = d113data,
                                       causeids = c(16, 96),
                                       cause = NULL,
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)),
      suppressWarnings(death_113_count(ph.data = d113data,
                                       causeids = c(1:113),
                                       cause = c("malaria", "motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)))
    expect_equal(
      suppressWarnings(death_113_count(ph.data = d113data,
                                       causeids = c(16, 96),
                                       cause = c("malaria|motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)),
      suppressWarnings(death_113_count(ph.data = d113data,
                                       causeids = c(1:113),
                                       cause = c("malaria", "motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)))
  })

  test_that("Structure of output table is as expected ...", {
    expect_equal(nrow(d113res.default), 10) # eight causeids should be present, PLUS COVID, PLUS 'All causes'
    expect_equal(sort(names(d113res.default)), c("cause.of.death", "causeid", "deaths"))
    expect_equal(nrow(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65, death_age_col = "ageofdeath"))),
                 10) # eight causeids should be present, PLUS COVID, PLUS 'All causes'
    expect_equal(sort(names(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 85, death_age_col = "ageofdeath")))),
                 c("cause.of.death", "causeid", "deaths", "ypll_85"))
    expect_equal(names(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = 'cod.icd10', kingco = F, group_by = c('strata')))[]),
                 c('cause.of.death', 'causeid', 'deaths', 'strata'))
    expect_equal(sort(unique(suppressWarnings(death_113_count(ph.data = d113data2, icdcol = 'cod.icd10', kingco = F, group_by = c('strata')))[]$strata)),
                 c('one', 'three', 'two')) # ensure all strata are present
  })

# Check death_130 ----
  # death_130() create data ----
  # not necessary

  # death_130() create output ----
  # not necessary

  # death_130() tests ----
  test_that("Check that NCHS 130 cause ids are saved to data.table object and have correct structure ...", {
    d130ids <- death_130()
    expect_equal(nrow(d130ids), 130) # Should have 130 rows
    expect_equal(length(unique(d130ids$causeid)), 130)
    expect_equal(length(unique(d130ids$cause.of.death)), 130)
    expect_equal(names(d130ids), c("causeid", "cause.of.death"))
    expect_equal(d130ids, unique(d130ids))
  })


# Check death_130_count ----
  # death_130_count() create data ----
  set.seed(98104)
  # create synthetic line level data
  d130data <- data.table::data.table(
    cod.icd10 = c(rep("P36.3", round(runif(1, 30, 100000), 0)), # 85, Bacterial sepsis of newborn
                  rep("V022", round(runif(1, 30, 100000), 0)), # 114, Motor vehicle accidents
                  rep("P021", round(runif(1, 30, 100000), 0)), # 64, Newborn affected by complications involving placenta
                  rep("P202", round(runif(1, 30, 100000), 0)), #75, Intrauterine hypoxia
                  rep("I26", round(runif(1, 30, 100000), 0)), # 36, Pulmonary heart disease and diseases of pulmonary circulation
                  rep("R951", round(runif(1, 30, 100000), 0)), # 11, Sudden infant death syndrome
                  rep("P080", round(runif(1, 30, 100000), 0)), # 73, Disorders related to long gestation and high birth weight
                  rep("A09", round(runif(1, 30, 100000), 0)), # 2, Diarrhea and gastroenteritis of infectious origin
                  rep("P702", round(runif(1, 30, 100000), 0))) #92, Syndrome of infant of a diabetic mother and neonatal diabetes mellitus
  )

  d130data2 <- data.table::copy(d130data)
  set.seed(98104)
  d130data2[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
            1:nrow(d130data2)]

  # create reference table for testing (I know these values are correct because I chose the ICD10 #s)
  COD = data.table(cod = c("Bacterial sepsis of newborn",
                           "Motor vehicle accidents",
                           "Newborn affected by complications involving placenta",
                           "Intrauterine hypoxia",
                           "Pulmonary heart disease and diseases of pulmonary circulation",
                           "Sudden infant death syndrome",
                           "Disorders related to long gestation and high birth weight",
                           "Diarrhea and gastroenteritis of infectious origin",
                           "Syndrome of infant of a diabetic mother and neonatal diabetes mellitus"),
                   id = gsub("\\.", "", unique(d130data$cod.icd10)) )

  # death_130_count() create output ----
  d130res.default <- suppressWarnings(death_130_count(ph.data = d130data,
                                                      causeids = seq(1, 130, 1),
                                                      cause = NULL,
                                                      icdcol = "cod.icd10",
                                                      kingco = FALSE))

  d130res.manual <- copy(d130data)
  d130res.manual[, cod.icd10 := gsub("\\.", "", cod.icd10)]
  d130res.manual <- merge(d130res.manual, COD, by.x = "cod.icd10", by.y = "id", all = TRUE)
  d130res.manual <- d130res.manual[, .(manual.count = .N), cod]

  # death_130_count() tests ----
  test_that("Check for proper triggering of errors ...", {
    expect_error(death_130_count(ph.data = "d130data", kingco = FALSE)) # name of data.table must be unquoted
    expect_warning(death_130_count(ph.data = d130data, icdcol = "cod.icd10", kingco = FALSE)) # warning because of period in A85.2
    expect_error(suppressWarnings(death_130_count(ph.data = d130data, kingco = TRUE))) # Should error because there is no KC data here
    expect_error(death_130_count(ph.data = d130data, causeids = seq(1, 115, 1), kingco = FALSE)) # Should error because highest causeid is 114
    expect_error(death_130_count(ph.data = d130data, causeids = seq(0, 114, 1), kingco = FALSE)) # Should error because lowest causeid is 0
    expect_error(death_130_count(ph.data = d130data, causeids = c(2, 5, "7", 13), kingco = FALSE)) # Should error because of a non-numeric causeid
    expect_error(death_130_count(ph.data = d130data, causeids = c(2, 5, 7.3, 13), kingco = FALSE)) # Should error because of a non-integer causeid
    expect_error(death_130_count(ph.data = d130data, icdcol = cod.icd10, kingco = FALSE)) # Should error because icdcol should be quoted
    expect_error(death_130_count(ph.data = d130data, icdcol = "cod.icd10x", kingco = FALSE)) # Should error because icdcol does not exist
    expect_error(suppressWarnings(death_130_count(ph.data = d130data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = "65"))) # Should error because ypll_age is character
    expect_error(suppressWarnings(death_130_count(ph.data = d130data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65))) # Should error bc need to specify dob/dod or death_age_col
    expect_error(suppressWarnings(death_130_count(ph.data = d130data2, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65, death_age_col = ageofdeath))) # Should error ageofdeath not quoted
    # do not need to test group_by, because already tested with death_113_count, and both use same underlying function (death_xxx_count)
  })

  test_that("Death counts by cause are accurate ...", {
    expect_equal(nrow(d130res.default), nrow(d130res.manual) + 1 ) # 1 extra rows in function (cause.of.death == 'All causes')
    expect_equal(length(intersect(d130res.default$cause.of.death, d130res.manual$cod)), 9) # confirm names of causes of death
    expect_equal(sum(d130res.default[!cause.of.death %in% c('All causes')]$deaths),
                 sum(d130res.manual$manual.count)) # confirm count
    # do not need to test group_by, because already tested with death_113_count, and both use same underlying function (death_xxx_count)
  })

  test_that("'cause' argument works correctly ...", {
    expect_equal(
      suppressWarnings(death_130_count(ph.data = d130data,
                                       causeids = c(85, 114),
                                       cause = NULL,
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)),
      suppressWarnings(death_130_count(ph.data = d130data,
                                       causeids = c(1:130),
                                       cause = c("sepsis", "motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)))
    expect_equal(
      suppressWarnings(death_130_count(ph.data = d130data,
                                       causeids = c(1:30),
                                       cause = c("sepsis|motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)),
      suppressWarnings(death_130_count(ph.data = d130data,
                                       causeids = c(1:130),
                                       cause = c("sepsis", "motor vehicle"),
                                       icdcol = "cod.icd10",
                                       kingco = FALSE)))
  })

  test_that("Structure of output table is as expected ...", {
    expect_equal(nrow(d130res.default), 10) # eight causeids should be present, PLUS COVID, PLUS 'All causes'
    expect_equal(sort(names(d130res.default)), c("cause.of.death", "causeid", "deaths"))
    expect_equal(nrow(suppressWarnings(death_130_count(ph.data = d130data2,
                                                       icdcol = "cod.icd10",
                                                       kingco = FALSE,
                                                       ypll_age = 65,
                                                       death_age_col = "ageofdeath"))),
                 10) # eight causeids should be present, PLUS COVID, PLUS 'All causes'
    expect_equal(sort(names(suppressWarnings(death_130_count(ph.data = d130data2,
                                                             icdcol = "cod.icd10",
                                                             kingco = FALSE,
                                                             ypll_age = 85,
                                                             death_age_col = "ageofdeath")))),
                 c("cause.of.death", "causeid", "deaths", "ypll_85"))
    # do not need to test group_by, because already tested with death_113_count, and both use same underlying function (death_xxx_count)
  })

# Check death_other ----
  # death_other create data ----
  # not necessary

  # death_other create output ----
  # not necessary

  # death_other tests ----
  test_that("Check that death_other() exists, is the right type, and has some expected values ...", {
    otherz <- death_other()
    expect_true(is.character(otherz))
    expect_true(any(grepl('opioid', otherz, ignore.case = T)))
    expect_true(any(grepl('alcohol', otherz, ignore.case = T)))
    expect_true(any(grepl('overdose', otherz, ignore.case = T)))
  })

# Check death_other_count ----
  # death_other_count create data ----
  set.seed(98104)
  otherdata <- data.table::data.table(cod.icd10 = sample(rads.data::icd_other_causes_of_death[]$icd10, size = 5000, replace = T),
                                      chi_age = sample(35:85, 5000, replace = T),
                                      shape = sample(c('square', 'cirlce', 'triangle'), size = 5000, replace = T),
                                      binary = sample(c('On', 'Off'), size = 5000, replace = T))
  otherdata[, ypll_65 := fifelse(chi_age < 65, 65 - chi_age, 0)]


  otherdata.manual <- merge(otherdata,
                     rads.data::icd_other_causes_of_death[, .(cause.of.death, cod.icd10 = icd10)],
                     by = 'cod.icd10',
                     all.x = T,
                     all.y = F) # will lengthen table because some ICD codes map to > 1 'Other' cause of death
  otherdata.manual[, ypll_65 := fifelse(chi_age < 65, 65 - chi_age, 0)]

  # death_other_count create output ----
  other.rads <- suppressWarnings(death_other_count(ph.data = copy(otherdata)[, ypll_65 := NULL],
                                                   cause = death_other(),
                                                   icdcol = "cod.icd10",
                                                   kingco = FALSE,
                                                   group_by = c('shape', 'binary'),
                                                   ypll_age = 65,
                                                   death_age_col = 'chi_age'))
  other.manual <- rbind(
    # cause specific
    merge(otherdata.manual[, .(manual.count = .N), .(cause.of.death, shape, binary)],
          otherdata.manual[, .(manual.ypll = sum(ypll_65)), .(cause.of.death, shape, binary)],
          by = c('cause.of.death', 'shape', 'binary'),
          all = T),
    # all cause
    merge(otherdata[, .(manual.count = .N), .(shape, binary)],
          otherdata[, .(manual.ypll = sum(ypll_65)), .(shape, binary)],
          by = c('shape', 'binary'),
          all = T)[, cause.of.death := 'All causes']
  )

  other.rads <- other.rads[, .(cause.of.death, shape, binary, deaths, ypll_65)]
  other.manual <- other.manual[, .(cause.of.death, shape, binary, deaths = manual.count, ypll_65 = manual.ypll)]

  # death_other_count tests ----
    test_that("Check for proper triggering of errors ...", {
      ph.data <- data.table(underlying_cod_code = c("A00", "A01", "A02"),
                            chi_geo_kc = c("King County", "King County", "Other County"),
                            chi_age = c(65, 70, 75))

      # missing ph.data
      expect_error(death_other_count(cause = "A00"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # ph.data is a data.frame/data.table
      expect_error(death_other_count(ph.data = list(), cause = "A00"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # missing cause
      expect_error(death_other_count(ph.data = ph.data),
                   "\U0001f47f `cause` cannot be missing. Please specify the `cause = XXX` argument and submit again")

      # cause is a character vector
      expect_error(death_other_count(ph.data = ph.data, cause = 123),
                   "\U0001f47f `cause` must be a character vector with whole or partial keywords for the cause of death of interest.")

      # icdcol is in ph,data
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", icdcol = "invalid_column"),
                   "\U0001f47f `icdcol` must be the name of a column that exists in `ph.data`.")

      # kingco is a logical
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", kingco = "TRUE"),
                   "\U0001f47f `kingco` must be a logical value, i.e., TRUE or FALSE.")

      # missing chi_geo_kc when when kingco == T
      ph.data_no_kingco <- copy(ph.data)[, chi_geo_kc := NULL]
      expect_error(death_other_count(ph.data = ph.data_no_kingco, cause = "A00", kingco = TRUE),
                   "\U0001f47f `ph.data` does not have the column `chi_geo_kc`, which is required for King County data.")

      # valid group_by columns
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", group_by = c("invalid_column")),
                   "\U0001f6d1 The following `group_by` values are not column names in `ph.data`: invalid_column.")

      # valid ypll_age values
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", ypll_age = 0),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", ypll_age = 100),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", ypll_age = "10"),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")

      # valid death_age_col
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", ypll_age = 75, death_age_col = "invalid_column"),
                   "\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
      })

    test_that("Death counts & YPLL counts by cause are accurate ...", {
      expect_equal(dim(other.rads), dim(other.manual)) # table size
      expect_identical(names(other.rads), names(other.manual)) # col names
      expect_identical(sort(unique(other.rads$cause.of.death)), sort(unique(other.manual$cause.of.death))) # COD

      expect_equal(sum(other.rads[cause.of.death == 'All causes']$deaths), sum(other.manual[cause.of.death == 'All causes']$deaths)) # all cause deaths
      expect_equal(sum(other.rads[cause.of.death != 'All causes']$deaths), sum(other.manual[cause.of.death != 'All causes']$deaths)) # cause specific deaths

      expect_equal(sum(other.rads[cause.of.death == 'All causes']$ypll_65), sum(other.manual[cause.of.death == 'All causes']$ypll_65)) # all cause YPLL
      expect_equal(sum(other.rads[cause.of.death != 'All causes']$ypll_65), sum(other.manual[cause.of.death != 'All causes']$ypll_65)) # cause specific YPLL
    })

    test_that("'cause' argument works correctly ...", {
      expect_identical(
        sort(unique(death_other_count(ph.data = copy(otherdata)[, ypll_65 := NULL],
                          cause = 'heart disease',
                          icdcol = "cod.icd10",
                          kingco = FALSE,
                          group_by = c('shape', 'binary'),
                          ypll_age = 65,
                          death_age_col = 'chi_age')[]$cause.of.death)),
        c('All causes', 'Heart disease')
        )

        expect_identical(
          sort(unique(death_other_count(ph.data = copy(otherdata)[, ypll_65 := NULL],
                                        cause = c('heat', 'stress', 'drug'),
                                        icdcol = "cod.icd10",
                                        kingco = FALSE,
                                        group_by = c('shape', 'binary'),
                                        ypll_age = 65,
                                        death_age_col = 'chi_age')[]$cause.of.death)),
          c('All causes', 'Drug-induced', 'Drug-overdose', 'Drug_Death', 'HeatStress_Death')
        )
    })

# Check death_injury_matrix_count ----
  # death_injury_matrix_count() create data ----
  set.seed(98104)
  # create synthetic line level data
  injurydata <- data.table::data.table(
    cod.icd10 = c(rep("X99", round(runif(1, 30, 10000), 0)), # Cut/pierce, Homicide
                  rep("W65", round(runif(1, 30, 10000), 0)), # Drowning, Unintentional
                  rep("X80", round(runif(1, 30, 10000), 0)), # Fall, Suicide
                  rep("Y26", round(runif(1, 30, 10000), 0)), # Fire/flame, Undetermined
                  rep("Y350", round(runif(1, 30, 10000), 0)), # Firearm, Legal intervention/war
                  rep("X40", round(runif(1, 30, 10000), 0)), # Poisoning, Unintentional
                  rep("X50", round(runif(1, 30, 10000), 0)), # Overexertion, Unintentional
                  rep("Y03", round(runif(1, 30, 10000), 0)), # Other land transport, Homicide
                  rep("V10", round(runif(1, 30, 10000), 0))) # Pedal cyclist, other, Unintentional
  )

  # create copy of synthetic data with an age of death
  injurydata4 <- copy(injurydata)
  injurydata4[, year := sample(2015:2020, nrow(injurydata), replace = TRUE)]
  set.seed(98104)
  injurydata4[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0), 1:nrow(injurydata4)] # synthetic age of death

  # create a copy of synthetic data with date_of_birth and date_of_death
  injurydata5 <- copy(injurydata4)
  set.seed(98104)
  injurydata5[, date_of_death := as.Date(paste0(year, "-", sample(1:12, 1, replace = T), "-", sample(1:28, 1, replace = T)) ), 1:nrow(injurydata5)] # synthetic date_of_death
  injurydata5[, date_of_birth := date_of_death - (365*ageofdeath + sample(1:360, 1, replace = T))] # synthetic date_of_birth
  injurydata5[, c("ageofdeath") := NULL]

  injurydata5[, strata := sample(c("one", "two", "three"), size = nrow(injurydata5), replace = T)]


  minimatrix <- data.table(
    cod.icd10 = unique(injurydata$cod.icd10),
    mechanism = c("Cut/pierce", "Drowning", "Fall", "Fire/flame", "Firearm", "Poisoning", "Overexertion", "Other land transport", "Pedal cyclist, other"),
    intent = c("Homicide", "Unintentional", "Suicide", "Undetermined", "Legal intervention/war", "Unintentional", "Unintentional", "Homicide", "Unintentional")
  )

  # death_injury_matrix_count() create output ----
  # manually
  injuries_m <- injurydata[, .(deaths = .N), "cod.icd10"]
  injuries_m <- merge(injuries_m, minimatrix, by = "cod.icd10", all = FALSE)
  injuries_m <- rbind(injuries_m[, .(mechanism = "All injury", deaths = sum(deaths)), "intent"], injuries_m, fill = TRUE)
  setcolorder(injuries_m, c("mechanism", "intent", "deaths"))

  # using the function
  injuries_f <- death_injury_matrix_count(ph.data = injurydata,
                                     intent = "*",
                                     mechanism = "*",
                                     icdcol = "cod.icd10",
                                     kingco = F)
  injuries_f <- injuries_f[!mechanism %in% c("All transport", "Fire/hot object or substance")] # These are summary categories

  # death_injury_matrix_count() tests ----
  test_that("Check for proper triggering of errors ...", {
    expect_error(death_injury_matrix_count(ph.data = "injurydata", kingco = FALSE)) # name of data.table must be unquoted
    expect_error(death_injury_matrix_count(ph.data = injurydatum, kingco = FALSE)) # warning because data.table doesn't exist
    expect_error(suppressWarnings(death_injury_matrix_count(ph.data = injurydata, kingco = TRUE))) # Should error because there is no KC data here
    expect_error(death_injury_matrix_count(ph.data = injurydata, intent = "z", kingco = FALSE)) # Should error because none of the intents have 'z'
    expect_error(death_injury_matrix_count(ph.data = injurydata, mechanism = "z", kingco = FALSE)) # Should error because none of the mechanisms have 'z'
    expect_error(death_injury_matrix_count(ph.data = injurydata, intent = 100, kingco = FALSE)) # Should error because intent must be a character
    expect_error(death_injury_matrix_count(ph.data = injurydata, mechanism = 100, kingco = FALSE)) # Should error because mechanism must be a character
    expect_error(death_injury_matrix_count(ph.data = injurydata, icdcol = cod.icd10, kingco = FALSE)) # Should error because icdcol should be quoted
    expect_error(death_injury_matrix_count(ph.data = injurydata, icdcol = "cod.icd10x", kingco = FALSE)) # Should error because icdcol does not exist
    expect_error(death_injury_matrix_count(ph.data = injurydata, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65)) # Should error because data lacks dob/dod
    expect_error(death_injury_matrix_count(ph.data = injurydata4, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65)) # Should error because didn't specify age
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, ypll_age = "65")) # Should error because ypll_age is character
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, ypll_age = 65.1)) # Should error because ypll_age is not integer
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, group_by = 'stratum')) # Should error because colunmn is `strata`, not stratum
  })

  test_that("Filtering by intent and mechanism work properly ...", {
    intent.check <- death_injury_matrix_count(ph.data = injurydata,
                                              intent = "suicide",
                                              mechanism = "*",
                                              icdcol = "cod.icd10",
                                              kingco = F)
    mechanism.check <- death_injury_matrix_count(ph.data = injurydata,
                                                 intent = "*",
                                                 mechanism = "firearm",
                                                 icdcol = "cod.icd10",
                                                 kingco = F)
    double.none <- suppressWarnings(death_injury_matrix_count(ph.data = injurydata,
                                                              intent = "none",
                                                              mechanism = "none",
                                                              icdcol = "cod.icd10",
                                                              kingco = F))
    expect_equal(nrow(intent.check), 2) # 1 for Fall/suicide and 1 for All injury/suicide
    expect_equal(nrow(mechanism.check), 5) # the '*' gets all five intents
    expect_equal(nrow(double.none), 1) # All injury/Any intent
  })

  test_that("Death counts are accurate ...", {
    expect_equal(sum(injuries_f[mechanism == "All injury"]$deaths), sum(injuries_m[mechanism == "All injury"]$deaths)) # summary by intent
    expect_equal(sum(injuries_f[mechanism != "All injury"]$deaths), sum(injuries_m[mechanism != "All injury"]$deaths)) # individual mechanisms
    expect_equal(sum(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, group_by = 'strata')[]$deaths),
                 sum(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE)[]$deaths)) # group_by should not impact total
  })

  test_that("YPLL counts are accurate ...", {
    manual5 <- copy(injurydata5)[, AGEz := rads::calc_age(date_of_birth, date_of_death)] # manually calc age
    manual5[AGEz <= 65, ypll_65 := 65 - AGEz]

    # compare to manually calculated YPLL_65
    expect_equal(sum(suppressWarnings(death_injury_matrix_count(ph.data = injurydata4[year == 2020],
                                               intent = "none",
                                               mechanism = "none",
                                               icdcol = "cod.icd10",
                                               kingco = FALSE,
                                               ypll_age = 65,
                                               death_age_col = "ageofdeath"))[]$ypll_65),
                 sum(manual5[year == 2020]$ypll_65, na.rm = T))

  })

  test_that("Structure of output table is as expected ...", {
    expect_equal(nrow(injuries_f), 50) # there are 10 mechanisms (9 unique + 'All injury') X 5 intents
    expect_equal(nrow(injuries_f[deaths != 0]), 14) # there are only 14 valid combinations of mechanism and intent in this data
    expect_equal(sort(names(injuries_f)), c("deaths", "intent", "mechanism"))
    expect_equal(sort(names(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, group_by = 'strata'))),
                 sort(c('mechanism', 'intent', 'deaths', 'strata')))
    expect_equal(sort(unique(death_injury_matrix_count(ph.data = injurydata5, icdcol = "cod.icd10", kingco = FALSE, group_by = 'strata')[]$strata)),
                 sort(c('one', 'two', 'three')))
  })

# Check death_icd10_clean ----
  # Test for proper conversion ----
  test_that("ICD-10 codes are correctly cleaned and standardized", {
    expect_equal(suppressWarnings(death_icd10_clean(c("A85.2"))), "A852")
    expect_equal(suppressWarnings(death_icd10_clean(c("b99-1"))), "B991")
    expect_equal(suppressWarnings(death_icd10_clean(c("C34"))), "C340")
    expect_equal(suppressWarnings(death_icd10_clean(c("J20.9"))), "J209")
  })

  # Test for handling of NA values for invalid patterns ----
  test_that("Invalid ICD-10 patterns are set to NA", {
    expect_equal(suppressWarnings(death_icd10_clean(c("1234"))), NA_character_)
    expect_equal(suppressWarnings(death_icd10_clean(c("ABCDE"))), NA_character_)
  })

  # Test for errors on missing input  ----
  test_that("Error is thrown for missing input", {
    expect_error(death_icd10_clean(), "cannot be missing")
  })

  # Test for warnings ----
  test_that("Warning is issued for non-alphanumeric characters", {
    expect_warning(death_icd10_clean(c("A85.2")), "non alpha-numeric character")
    expect_warning(death_icd10_clean(c("B99-1")), "non alpha-numeric character")
    expect_warning(death_icd10_clean(c("1X12")), "have been replaced with NA")
    expect_warning(death_icd10_clean(c("X12X")), "have been replaced with NA")
  })

  # Test for specific lengths and padding ----
  test_that("Codes are trimmed or padded to 4 characters", {
    input <- c("A1", "B99", "C123", "D1234", "E12345")
    expected <- c("A100", "B990", "C123", "D123", "E123")
    result <- suppressWarnings(death_icd10_clean(input))
    expect_equal(result, expected)
  })

# Check life_table ----
  # life_table() create data ----
  # Test with 1970 CA Abridged Death Data
  # Chiang, Chin Long & World Health Organization. (1979).
  # Life table and mortality analysis / Chin Long Chiang.
  # World Health Organization. https://apps.who.int/iris/handle/10665/62916
  dt <- data.table::data.table(
    ages = c("0-1", "1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
             "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
             "70-75", "75-80", "80-85", "85+"),
    deaths = c(6234, 1049, 723, 735, 2054, 2702, 2071, 1964, 2588, 4114, 6722,
               8948, 11942, 14309, 17088, 19149, 21325, 20129, 22483),
    pop = c(340483, 1302198, 1918117, 1963681, 1817379, 1740966, 1457614,
            1219389, 1149999, 1208550, 1245903, 1083852, 933244, 770770,
            620805, 484431, 342097, 210953, 142691),
    fraction = c(0.09, 0.41, 0.44, 0.54, 0.59, 0.49, 0.51, 0.52, 0.53, 0.54, 0.53,
                 0.53, 0.52, 0.52, 0.51, 0.52, 0.51, 0.50, NA))

  # life_table() create output ----
  # default argument values
  test1 <- life_table(ph.data = dt,
                      myages = "ages",
                      mydeaths = "deaths",
                      mypops = "pop",
                      myprops = "fraction",
                      ci = 0.95)

  # when argument have non-default values
  dt2 <- copy(dt)
  setnames(dt2, paste0(names(dt), "x"))
  test2 <- life_table(ph.data = dt2,
                      myages = "agesx",
                      mydeaths = "deathsx",
                      mypops = "popx",
                      myprops = "fractionx",
                      ci = 0.95)
  setnames(test2, c("agesx", "popx", "deathsx", "fractionx"), c("ages", "pop", "deaths", "fraction"))

  # alternate ci
  test1.90 <- life_table(ph.data = dt,
                         myages = "ages",
                         mydeaths = "deaths",
                         mypops = "pop",
                         myprops = "fraction",
                         ci = 0.90)
  test1.99 <- life_table(ph.data = dt,
                         myages = "ages",
                         mydeaths = "deaths",
                         mypops = "pop",
                         myprops = "fraction",
                         ci = 0.99)

  # life_table() tests ----
  test_that("Check for errors based on validation failure...", {
    expect_error(life_table()) # need to specify data.frame
    expect_error(life_table(hello)) # non-existant data.frame
    expect_error(life_table(dt, ages = "blah"))
    dta<-copy(dt); dta[, ages := gsub("-", "_", ages)]
    expect_error(life_table(dta)) # interval needs '-'
    dta<-copy(dt); dta[, ages := gsub("\\+", "", ages)]
    expect_error(life_table(dta)) # final interval needs '+'
    dta<-copy(dt); dta[, pop := as.character(pop)]
    expect_error(life_table(dta)) # pop must be numeric
    dta<-copy(dt); dta[, deaths := as.character(deaths)]
    expect_error(life_table(dta)) # deaths must be numeric
    dta<-rbind(copy(dt), data.table(ages = c(NA, NA), deaths = c(1000, 1000)), fill = T)
    expect_error(life_table(dta)) # ages can only have one row with NA
    dta<-copy(dt); dta[, fraction := as.character(fraction)]
    expect_error(life_table(dta)) # my_frac must be numeric
    expect_error(life_table(dt, ci = 1 )) # ci must be between 0.01 & 0.99
    expect_error(life_table(dt, ci = 0 )) # ci must be between 0.01 & 0.99
    expect_error(life_table(dt, ci = -.1 )) # ci must be between 0.01 & 0.99
  })

  test_that('Confirm output is independent of argument/column names...',{
    expect_equal( test1, test2)
  })

  test_that('Confirm that group_by argument works when specified...',{
    # Create arbitrary small variations for 'demographic' groups ----
      # first create an empty table
        mygroups <- data.table::CJ(shape = c('circle', 'square'), color = c('blue', 'orange'))
        dt_groups <- merge(copy(dt)[,constant := 1],
                           mygroups[, constant := 1],
                           by = 'constant',
                           allow.cartesian = T)[, constant := NULL]
      # now modify the values
        set.seed(98104)
        dt_groups[, deaths := round(deaths * sample(seq(.75, 1.25, .01), .N, replace = TRUE))]
        dt_groups[, pop := round(pop * sample(seq(.75, 1.25, .01), .N, replace = TRUE))]

      # append the original data
        dt_groups2 <- rbind(dt_groups, dt, fill = T)

    # Run the tests ----
      expect_no_error(test_groups <- life_table(ph.data = dt_groups,
                                                myages = 'ages',
                                                mydeaths = 'deaths',
                                                mypops = 'pop',
                                                myprops = 'fraction',
                                                group_by = c('shape', 'color'),
                                                ci = 0.95))
      expect_equal(nrow(test_groups), 76) # 76 because 4 stratum and 19 age groups

      expect_no_error(test_groups2 <- life_table(ph.data = dt_groups2,
                                                 myages = 'ages',
                                                 mydeaths = 'deaths',
                                                 mypops = 'pop',
                                                 myprops = 'fraction',
                                                 group_by = c('shape', 'color'),
                                                 ci = 0.95))
      expect_identical(test1, # original data, run by itself
                       test_groups2[is.na(shape) & is.na(color)][, c('shape', 'color') := NULL] # original data when run with other groups
                       )
  })


  test_that('structure and results compared to Chiang 1979...',{
    expect_equal( nrow(test1), 19)
    expect_equal( ncol(test1), 15)
    expect_equal( test1[1]$qx, 0.01801)
    expect_equal( test1[1]$ex, 71.95)
    expect_equal( test1[10]$qx, 0.01689)
    expect_equal( test1[10]$ex, 35.56)
    expect_equal( test1[19]$qx, 1)
    expect_equal( test1[19]$ex, 6.35)
  })

  test_that('confidence intervals seem logical...',{
    # remember the higher the % confidence, the wider the interval
    expect_gt( test1.99[7]$ex_upper, test1[7]$ex_upper)
    expect_gt( test1[7]$ex_upper, test1.90[7]$ex_upper)
    expect_lt( test1.99[7]$ex_lower, test1[7]$ex_lower)
    expect_lt( test1[7]$ex_lower, test1.90[7]$ex_lower)
  })

  test_that('check that deaths with an unknown age interval and redistributed...', {
    dtna <- rbind(dt, data.table(deaths = 16000), fill = T)
    dtna_table <- life_table(dtna)
    expect_equal(nrow(dtna_table), nrow(test1))
    expect_lte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 + 3)) # allow some buffer for rounding
    expect_gte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 - 3)) # allow some buffer for rounding

    dtna <- rbind(dt, data.table(deaths = rep(16000, 2)), fill = T)
    expect_error( life_table(dtna)) # should not allow more than 1 row with deaths and missing age interval

    dtna <- rbind(dt2, data.table(deathsx = 16000), fill = T)
    dtna_table <- life_table(dtna, myages = "agesx", mydeaths = "deathsx", mypops = "popx", myprops = "fractionx")
    expect_equal(nrow(dtna_table), nrow(test1))
    expect_lte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 + 3)) # allow some buffer for rounding
    expect_gte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 - 3)) # allow some buffer for rounding
  })

# Check life_table_predict_mx ----
  # life_table_predict_mx is used by life_table when have missing or zero deaths
  # in the oldest age group. Best to simply test this scenario with life_table
  # Create data ----
    # complete table
      dt <- data.table(
        shape = c(rep("Square", 20), rep("Circle", 20)),
        ages = c("0-1", "1-5", "10-15", "15-18", "18-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "5-10",
                 "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85+",
                 "0-1", "1-5", "10-15", "15-18", "18-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "5-10",
                 "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85+"),
        deaths = c(212, 28, 26, 42, 29, 102, 169, 216, 325, 369, 538, 19, 881, 1309, 1712, 2225, 2679, 3016, 3946, 14957,
                   268, 34, 40, 87, 112, 393, 495, 522, 591, 647, 917, 22, 1410, 2136, 2869, 3385, 3555, 3573, 3948, 9436),
        fraction = c(0.06139045, 0.24805452, 0.51783137, 0.42818004, 0.37984224, 0.45175494, 0.42909058, 0.45478232, 0.43024378,
                     0.44906213, 0.46987587, 0.35951133, 0.44678210, 0.43631448, 0.42537686, 0.43599385, 0.42606675, 0.42873223,
                     0.43436675, 0.45719584, 0.05598660, 0.40372919, 0.46953323, 0.45510943, 0.33719204, 0.42685206, 0.43477724,
                     0.41514970, 0.41599625, 0.42559335, 0.44192055, 0.47201290, 0.45655819, 0.43510860, 0.43777322, 0.42284555,
                     0.41961828, 0.41960811, 0.42241863, 0.37966882),
        pop = c(60317.00, 242150.33, 303483.82, 180610.16, 125693.81, 350694.40, 465846.22, 477813.49, 431277.65, 371051.13,
                370453.81, 308085.24, 351226.56, 341058.78, 312385.24, 258675.01, 197510.39, 130167.18, 89999.79, 115149.40,
                62738.56, 253080.73, 320067.23, 187910.53, 128870.42, 364203.13, 498792.04, 505409.47, 451411.41, 379740.65,
                379353.86, 323939.56, 357230.78, 342488.85, 298674.16, 235700.26, 166653.90, 105379.45, 65184.64, 63682.87)
      )

    # append copy with missing / no deaths for oldest age group
    dt <- rbind(dt,
                copy(dt)[, shape := paste0(shape, "2")][ages == '85+', c('deaths', 'fraction') := 0])

  # Create output ----
    mylifetable <- suppressWarnings(life_table(ph.data = dt, group_by = 'shape'))
    setorder(mylifetable, ages, shape)

  # Tests ----
    test_that("Check messages, proper filling of missing values, and LE0 estimates ...", {
      expect_warning(life_table(ph.data = dt, group_by = 'shape'), "Zero deaths detected")
      expect_equal(nrow(mylifetable[deaths == 0]), 2)
      expect_equal(nrow(mylifetable[is.na(mx)]), 0)
      expect_equal(nrow(mylifetable[is.na(ex)]), 0)
      expect_equal(nrow(mylifetable[is.na(ex_lower)]), 0)
      expect_lt(abs(mylifetable[ages == '0-1' & shape == 'Circle']$ex - mylifetable[ages == '0-1' & shape == 'Circle2']$ex), 2) # life expectancy within 2 years of the truth
      expect_lt(abs(mylifetable[ages == '0-1' & shape == 'Square']$ex - mylifetable[ages == '0-1' & shape == 'Square2']$ex), 2) # life expectancy within 2 years of the truth
    })

# Check life_table_prep ----
  # life_table_prep() create data ----
  set.seed(98104)
  ltp <- data.table::data.table(
    date_of_death = rep(as.Date("2020-01-01"), 10000) + sample(0:365, 10000, replace = TRUE),
    days_lived = round2(rnorm(10000, mean = 29930, sd = 11000), 0),
    race_eth = rep_len(c("AIAN", "Asian", "Black", "Hispanic", "NHPI", "White"), 1000),
    year = 2020
  )
  ltp[days_lived <0, days_lived := 0] # can't live negative days
  ltp[days_lived >43800, days_lived := 365*sample(35:100, 1)] # cap lifespan at 120 years
  ltp[, date_of_birth := date_of_death - days_lived]
  ltp[, days_lived := NULL]

  # life_table_prep() create output ----
  ltp_output <- life_table_prep(ph.data = ltp)
  ltp_output_group <- life_table_prep(ph.data = ltp, group_by = c('year', 'race_eth'))

  # life_table_prep() tests ----
  test_that("Check for errors based on validation failure...", {
    expect_error(life_table_prep()) # need to specify data.frame
    expect_error(life_table_prep("ltp")) # improper quoting of table name
    expect_error(life_table_prep(hello)) # non-existant data.frame
    expect_error(life_table_prep(ltp, cuts = NULL))
    expect_error(life_table_prep(ltp, cuts = c(0, NA, 100)))
    expect_error(life_table_prep(ltp, cuts = c(-1, 50, 100)))
    expect_warning(life_table_prep(ltp, cuts = c(0, 50, 105)))
    expect_error(life_table_prep(ltp, cuts = c("0", "50", "100")))
    expect_error(life_table_prep(ltp, dobvar = "dob")) # non-existent
    expect_error(life_table_prep(ltp, dodvar = "dod")) # non-existent
    expect_warning(life_table_prep(ltp, dobvar = "date_of_death", dodvar = "date_of_birth")) # date of death should not greater than date of birth
  })

  test_that("Confirm proper columns are output ...", {
    expect_equal(
      length(setdiff(names(ltp_output), c("ages", "deaths", "fraction"))),
      0 # expect zero because ages, deaths, and fractions should be the only columns in ltp_output
    )
  })

  test_that("Check that dates can be formatted as character vars ...", {
    ltp2 <- copy(ltp)[, c("date_of_birth", "date_of_death") := lapply(.SD, function(X){as.character(X)}), .SDcols = c("date_of_birth", "date_of_death")]
    expect_equal(life_table_prep(ltp), life_table_prep(ltp2))
  })

  test_that("Check that group_by command works as expected ...", {
    expect_identical(
      sort(setdiff(names(ltp), c('date_of_death', 'date_of_birth'))),
      sort(setdiff(names(ltp_output_group), c('ages', 'deaths', 'fraction')))
    )
    expect_identical(
      nrow(ltp),
      sum(ltp_output_group$deaths)
    )
    expect_identical(
      setorder(unique(ltp[, .(year, race_eth)]), race_eth, year),
      setorder(unique(ltp_output_group[, .(year, race_eth)]), race_eth, year)
    )
  })

  test_that("Provides complete table of demographics and ages ...", {
    # artificially create zero deaths for senior Hispanics
    ltp_alt <- ltp
    ltp_alt[race_eth == 'Hispanic', race_eth := fifelse(calc_age(date_of_birth, date_of_death) >= 64, 'White', race_eth)]

    # run
    ltp_output_group_alt <- life_table_prep(ph.data = ltp, group_by = c('year', 'race_eth'))

    # test
    expect_identical(ltp_output_group_alt[race_eth == 'Hispanic' & ages == '85+']$deaths, 0L)
  })
