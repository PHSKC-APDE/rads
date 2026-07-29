library(data.table)

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
    deathDT <- rads.data::synthetic_death

  # death_113_count() create output ----
    cod.of.interest <- c("Arthropod-borne viral encephalitis",
                         "Malaria",
                         "Malnutrition",
                         "Other nutritional deficiencies",
                         "Meningitis",
                         "Influenza",
                         "Pneumonia",
                         "Motor vehicle crash")

    cod.of.interest.ref <- rads.data::icd_nchs113causes[cause.of.death %in% cod.of.interest,
                                                        list(cause.of.death, underlying_cod_code = icd10)]

    d113res.manual <- merge(deathDT,
                            cod.of.interest.ref,
                            by = "underlying_cod_code",
                            all.x = TRUE, all.y = F)[!is.na(cause.of.death)]
    d113res.manual <- d113res.manual[, .(manual.count = .N), cause.of.death]


    d113res.default <- suppressWarnings(death_113_count(ph.data = deathDT,
                                                        causeids = NULL,
                                                        cause = cod.of.interest,
                                                        icdcol = "underlying_cod_code"))
    d113res.default <- d113res.default[cause.of.death %in% cod.of.interest]


  # death_113_count() tests ----
  test_that("Check for proper triggering of errors ...", {
    expect_error(death_113_count(ph.data = "deathDT")) # name of data.table must be unquoted
    expect_error(death_113_count(ph.data = deathDT, icdcol = "cod.icd10")) # warning because of period in A85.2
    expect_error(death_113_count(ph.data = deathDT, causeids = seq(1, 115, 1))) # Should error because highest causeid is 114
    expect_error(death_113_count(ph.data = deathDT, causeids = seq(0, 114, 1))) # Should error because lowest causeid is 0
    expect_error(death_113_count(ph.data = deathDT, causeids = c(2, 5, "7", 13))) # Should error because of a non-numeric causeid
    expect_error(death_113_count(ph.data = deathDT, causeids = c(2, 5, 7.3, 13))) # Should error because of a non-integer causeid
    expect_error(death_113_count(ph.data = deathDT, icdcol = cod.icd10)) # Should error because icdcol should be quoted
    expect_error(death_113_count(ph.data = deathDT, icdcol = "cod.icd10x")) # Should error because icdcol does not exist
    expect_error(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = "65"))) # Should error because ypll_age is character
    expect_error(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = 65))) # Should error bc need to specify dob/dod or death_age_col
    expect_error(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = 65, death_age_col = ageofdeath))) # Should error ageofdeath not quoted
    expect_error(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = 'cod.icd10', by = c('stratum')))) # stratum does not exist, should be `strata`
  })

  test_that("Death counts by cause are accurate ...", {
    expect_equal(nrow(d113res.default), nrow(d113res.manual))
    expect_equal(length(intersect(d113res.default$cause.of.death, d113res.manual$cause.of.death)), 6) # confirm names of causes of death
    expect_equal(sum(d113res.default$deaths), sum(d113res.manual$manual.count)) # confirm count

    expect_equal(sum(death_113_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament'))[cause.of.death %in% cod.of.interest]$deaths),
                 sum(d113res.default$deaths)) # confirm no changes in total deaths when using by (all cause)
  })

  test_that("'cause' argument works correctly ...", {
    expect_equal(
      suppressWarnings(death_113_count(ph.data = deathDT,
                                       causeids = c(16, 96),
                                       cause = NULL,
                                       icdcol = "underlying_cod_code")),
      suppressWarnings(death_113_count(ph.data = deathDT,
                                       causeids = c(1:113),
                                       cause = c("malaria", "motor vehicle"),
                                       icdcol = "underlying_cod_code")))
    expect_equal(
      suppressWarnings(death_113_count(ph.data = deathDT,
                                       causeids = c(16, 96),
                                       cause = c("malaria|motor vehicle"),
                                       icdcol = "underlying_cod_code")),
      suppressWarnings(death_113_count(ph.data = deathDT,
                                       causeids = c(1:113),
                                       cause = c("malaria", "motor vehicle"),
                                       icdcol = "underlying_cod_code")))
  })

  test_that("Structure of output table is as expected ...", {
    expect_equal(nrow(d113res.default), 6) # because zero malaria and anthropod-borne viral deaths
    expect_equal(sort(names(d113res.default)), c("cause.of.death", "causeid", "deaths"))
    expect_equal(nrow(death_113_count(ph.data = deathDT, icdcol = "underlying_cod_code", cause = cod.of.interest, ypll_age = 65, death_age_col = "age")),
                 8) # six causeids should be present, PLUS COVID, PLUS 'All causes'
    expect_equal(sort(names(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = "underlying_cod_code", ypll_age = 85, death_age_col = "age")))),
                 c("cause.of.death", "causeid", "deaths", "ypll_85"))
    expect_equal(names(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament')))[]),
                 c('cause.of.death', 'causeid', 'deaths', 'temperament'))
    expect_equal(sort(unique(suppressWarnings(death_113_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament')))[]$temperament)),
                 c('Active', 'Calm', 'Moderate')) # ensure all strata are present
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
    deathDT <- rads.data::synthetic_death

  # death_130_count() create output ----
    cod.of.interest <- c("Bacterial sepsis of newborn",
                         "Motor vehicle accidents",
                         "Newborn affected by complications involving placenta",
                         "Intrauterine hypoxia",
                         "Pulmonary heart disease and diseases of pulmonary circulation",
                         "Sudden infant death syndrome",
                         "Disorders related to long gestation and high birth weight",
                         "Diarrhea and gastroenteritis of infectious origin",
                         "Syndrome of infant of a diabetic mother and neonatal diabetes mellitus")

    cod.of.interest.ref <- rads.data::icd_nchs130causes[cause.of.death %in% cod.of.interest,
                                                        list(cause.of.death, underlying_cod_code = icd10)]


    d130res.manual <- merge(deathDT,
                            cod.of.interest.ref,
                            by = "underlying_cod_code",
                            all.x = TRUE, all.y = F)[!is.na(cause.of.death)]
    d130res.manual <- d130res.manual[, .(manual.count = .N), cause.of.death]


    d130res.default <- suppressWarnings(death_130_count(ph.data = deathDT,
                                                        causeids = NULL,
                                                        cause = cod.of.interest,
                                                        icdcol = "underlying_cod_code"))
    d130res.default <- d130res.default[cause.of.death %in% cod.of.interest]

  # death_130_count() tests ----
    test_that("Check for proper triggering of errors ...", {
      expect_error(death_130_count(ph.data = "deathDT")) # name of data.table must be unquoted
      expect_error(death_130_count(ph.data = deathDT, icdcol = "cod.icd10")) # warning because of period in A85.2
      expect_error(death_130_count(ph.data = deathDT, causeids = seq(1, 150, 1))) # Should error because highest causeid is 130
      expect_error(death_130_count(ph.data = deathDT, causeids = seq(0, 114, 1))) # Should error because lowest causeid is 0
      expect_error(death_130_count(ph.data = deathDT, causeids = c(2, 5, "7", 13))) # Should error because of a non-numeric causeid
      expect_error(death_130_count(ph.data = deathDT, causeids = c(2, 5, 7.3, 13))) # Should error because of a non-integer causeid
      expect_error(death_130_count(ph.data = deathDT, icdcol = cod.icd10)) # Should error because icdcol should be quoted
      expect_error(death_130_count(ph.data = deathDT, icdcol = "cod.icd10x")) # Should error because icdcol does not exist
      expect_error(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = "65"))) # Should error because ypll_age is character
      expect_error(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = 65))) # Should error bc need to specify dob/dod or death_age_col
      expect_error(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = "cod.icd10", ypll_age = 65, death_age_col = ageofdeath))) # Should error ageofdeath not quoted
      expect_error(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = 'cod.icd10', by = c('stratum')))) # stratum does not exist, should be `strata`
    })

    test_that("Death counts by cause are accurate ...", {
      expect_equal(nrow(d130res.default), nrow(d130res.manual))
      expect_equal(length(intersect(d130res.default$cause.of.death, d130res.manual$cause.of.death)), 8) # confirm names of causes of death
      expect_equal(sum(d130res.default$deaths), sum(d130res.manual$manual.count)) # confirm count

      expect_equal(sum(death_130_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament'))[cause.of.death %in% cod.of.interest]$deaths),
                   sum(d130res.default$deaths)) # confirm no changes in total deaths when using by (all cause)
    })

    test_that("'cause' argument works correctly ...", {
      expect_equal(
        suppressWarnings(death_130_count(ph.data = deathDT,
                                         causeids = c(85, 114),
                                         cause = NULL,
                                         icdcol = "underlying_cod_code")),
        suppressWarnings(death_130_count(ph.data = deathDT,
                                         causeids = c(1:130),
                                         cause = c("sepsis", "motor vehicle"),
                                         icdcol = "underlying_cod_code")))
      expect_equal(
        suppressWarnings(death_130_count(ph.data = deathDT,
                                         causeids = c(1:30),
                                         cause = c("sepsis|motor vehicle"),
                                         icdcol = "underlying_cod_code")),
        suppressWarnings(death_130_count(ph.data = deathDT,
                                         causeids = c(1:130),
                                         cause = c("sepsis", "motor vehicle"),
                                         icdcol = "underlying_cod_code")))
    })

    test_that("Structure of output table is as expected ...", {
      expect_equal(nrow(d130res.default), 8)
      expect_equal(sort(names(d130res.default)), c("cause.of.death", "causeid", "deaths"))
      expect_equal(nrow(death_130_count(ph.data = deathDT, icdcol = "underlying_cod_code", cause = cod.of.interest, ypll_age = 65, death_age_col = "age")),
                   10) # eight cod.of.interest should be present, PLUS COVID, PLUS 'All causes'
      expect_equal(sort(names(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = "underlying_cod_code", ypll_age = 85, death_age_col = "age")))),
                   c("cause.of.death", "causeid", "deaths", "ypll_85"))
      expect_equal(names(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament')))[]),
                   c('cause.of.death', 'causeid', 'deaths', 'temperament'))
      expect_equal(sort(unique(suppressWarnings(death_130_count(ph.data = deathDT, icdcol = 'underlying_cod_code', by = c('temperament')))[]$temperament)),
                   c('Active', 'Calm', 'Moderate')) # ensure all strata are present
    })

# Check death_multicause() ----
  # death_multicause() create data ----
  # not necessary

  # death_multicause() create output ----
  # not necessary

  # death_multicause() tests ----
  test_that("Check that death_multicause() exists, is the right type, and has some expected values ...", {
    multiz <- death_multicause()
    expect_true(inherits(multiz, 'data.table'))
    expect_identical(c('cause_name', 'description'), names(multiz))
    expect_true(grepl("opioid", multiz$cause_name, ignore.case = T))
  })

# Check death_multicause_count ----
  # death_multicause_count create data ----
    deathDT <- rbind(rads.data::synthetic_death, rads.data::synthetic_death)

    opioid_underlying <- c("X400", "X401", "X402", "X403", "X404", "X405", "X406", "X407", "X408", "X409", "X410",
                           "X411", "X412", "X413", "X414", "X415", "X416", "X417", "X418", "X419", "X420", "X421",
                           "X422", "X423", "X424", "X425", "X426", "X427", "X428", "X429", "X430", "X431", "X432",
                           "X433", "X434", "X435", "X436", "X437", "X438", "X439", "X440", "X441", "X442", "X443",
                           "X444", "X445", "X446", "X447", "X448", "X449", "X600", "X601", "X602", "X603", "X604",
                           "X605", "X606", "X607", "X608", "X609", "X610", "X611", "X612", "X613", "X614", "X615",
                           "X616", "X617", "X618", "X619", "X620", "X621", "X622", "X623", "X624", "X625", "X626",
                           "X627", "X628", "X629", "X630", "X631", "X632", "X633", "X634", "X635", "X636", "X637",
                           "X638", "X639", "X640", "X641", "X642", "X643", "X644", "X645", "X646", "X647", "X648",
                           "X649", "X850", "X851", "X852", "X853", "X854", "X855", "X856", "X857", "X858", "X859",
                           "Y100", "Y101", "Y102", "Y103", "Y104", "Y105", "Y106", "Y107", "Y108", "Y109", "Y110",
                           "Y111", "Y112", "Y113", "Y114", "Y115", "Y116", "Y117", "Y118", "Y119", "Y120", "Y121",
                           "Y122", "Y123", "Y124", "Y125", "Y126", "Y127", "Y128", "Y129", "Y130", "Y131", "Y132",
                           "Y133", "Y134", "Y135", "Y136", "Y137", "Y138", "Y139", "Y140", "Y141", "Y142", "Y143",
                           "Y144", "Y145", "Y146", "Y147", "Y148", "Y149")
    opioid_contributing <- c("T400", "T401", "T402", "T403", "T404", "T406")

  # death_multicause_count create output ----
    multi.rads <- death_multicause_count(ph.data = deathDT,
                                         cause_name = 'opioid',
                                         icdcol = "underlying_cod_code",
                                         contributing_cols = "record_axis_code",
                                         contributing_logic = "ANY",
                                         by = c('temperament'),
                                         ypll_age = 65,
                                         death_age_col = 'age')

  # death_multicause_count tests ----
    test_that("Check for proper triggering of errors ...", {
      # missing ph.data
      expect_error(death_multicause_count(cause_name = "opioid"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # ph.data is a data.frame/data.table
      expect_error(death_multicause_count(ph.data = list(), cause_name = "opioid"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # missing both cause_name and custom codes
      expect_error(death_multicause_count(ph.data = deathDT, underlying_codes = 'opioid'),
                   "\U0001f47f You must specify either `cause_name` OR both `underlying_codes`")

      # invalid cause_name
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = 123),
                   "\U0001f47f `cause_name` must be a single character value")

      # invalid contributing_logic
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", contributing_logic = "SOME"),
                   "\U0001f47f `contributing_logic` must be either 'ANY' or 'ALL'")

      # icdcol not in ph.data
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", icdcol = "invalid_column"),
                   "\n\U0001f47f `icdcol` \\('invalid_column'\\) was not found as a column in `ph.data`")

      # valid by columns
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", by = c("invalid_column")),
                   "\U0001f6d1 The following `by` values are not column names in `ph.data`")

      # valid ypll_age values
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", ypll_age = 0),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99")
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", ypll_age = 100),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99")

      # valid death_age_col
      expect_error(death_multicause_count(ph.data = deathDT, cause_name = "opioid", ypll_age = 75, death_age_col = "invalid_column"),
                   "\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`")
    })

    test_that("Check column names and ypll_65 ...", {
      expect_true(all(c('cause.of.death', 'deaths', 'temperament', 'ypll_65') %in% names(multi.rads))) # expected columns
      expect_true(all(multi.rads$ypll_65 >= 0)) # YPLL should be non-negative
    })

    test_that("'cause_name' vs custom codes arguments work correctly ...", {
      # Test with cause_name
      result1 <- death_multicause_count(ph.data = deathDT,
                                        cause_name = 'Opioid')

      # Test with custom codes
      result2 <- death_multicause_count(ph.data = deathDT,
                                        underlying_codes = opioid_underlying,
                                        contributing_codes = opioid_contributing,
                                        contributing_logic = "ANY")

      expect_identical(sort(unique(result2$cause.of.death)), c('All causes', 'Custom multicause'))
    })

    test_that("'contributing_logic' argument works correctly ...", {
      # Test ANY logic (default)
      result_any <- death_multicause_count(ph.data = deathDT,
                                           cause_name = 'opioid',
                                           contributing_logic = "ANY")

      # Test ALL logic
      result_all <- death_multicause_count(ph.data = deathDT,
                                           cause_name = 'opioid',
                                           contributing_logic = "ALL")

      # ALL logic should result in fewer deaths
      expect_true(result_all[cause.of.death == 'Opioid', deaths] <= result_any[cause.of.death == 'Opioid', deaths])
      expect_equal(result_all[cause.of.death == 'Opioid', deaths], 0) # would not expect opioid codes to be in ALL contributing cols
    })

    test_that("Alternative column naming works correctly ...", {
      # Create data with different column names
      alt_data <- data.table::copy(deathDT)
      data.table::setnames(alt_data, "underlying_cod_code", "underlyingCOD")
      data.table::setnames(alt_data, grep("record_axis_code", names(alt_data), value = T),
                           gsub("record_axis_code", "contributing_icd10", grep("record_axis_code", names(alt_data), value = T)))

      result.alt <- death_multicause_count(ph.data = alt_data,
                                       cause_name = 'opioid',
                                       icdcol = "underlyingCOD",
                                       contributing_cols = "contributing_icd10_")

      result.og <- death_multicause_count(ph.data = deathDT,
                                       cause_name = 'opioid',
                                       icdcol = "underlying_cod_code",
                                       contributing_cols = "record_axis_code")

      expect_identical(result.alt, result.og)
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
    deathDT <- rads.data::synthetic_death

  # death_other_count create output ----
    other.rads <- suppressWarnings(death_other_count(ph.data = deathDT,
                                                     cause = death_other(),
                                                     icdcol = "underlying_cod_code",
                                                     by = c('temperament'),
                                                     ypll_age = 65,
                                                     death_age_col = 'age'))[cause.of.death != 'All causes']

    cod.of.interest.ref <- rads.data::icd_other_causes_of_death[, list(cause.of.death, underlying_cod_code = icd10)]

    other.manual <- deathDT[cod.of.interest.ref,
                            on = "underlying_cod_code",
                            allow.cartesian = TRUE
    ][ , ypll_65 := fifelse(age < 65, 65 - age, 0)
    ][ , .(deaths = .N, ypll_65 = sum(ypll_65)), by = .(temperament, cause.of.death) ][!is.na(temperament)]


    other.rads <- other.rads[, .(cause.of.death, temperament, deaths, ypll_65)]
    other.manual <- other.manual[, .(cause.of.death, temperament, deaths, ypll_65)]

    setorder(other.rads, cause.of.death, temperament)
    setorder(other.manual, cause.of.death, temperament)

  # death_other_count tests ----
    test_that("Check for proper triggering of errors ...", {
      ph.data <- data.table(underlying_cod_code = c("A00", "A01", "A02"),
                            age = c(65, 70, 75))
      ph.data_clean <- copy(ph.data)[, underlying_cod_code := death_icd10_clean(underlying_cod_code)]

      # missing ph.data
      expect_error(death_other_count(cause = "A00"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # ph.data is a data.frame/data.table
      expect_error(death_other_count(ph.data = list(), cause = "A00"),
                   "\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")

      # missing cause
      expect_error(death_other_count(ph.data = ph.data_clean),
                   "\U0001f47f `cause` cannot be missing. Please specify the `cause = XXX` argument and submit again")

      # cause is a character vector
      expect_error(death_other_count(ph.data = ph.data_clean, cause = 123),
                   "\U0001f47f `cause` must be a character vector with whole or partial keywords for the cause of death of interest.")

      # icdcol is in ph,data
      expect_error(death_other_count(ph.data = ph.data, cause = "A00", icdcol = "invalid_column"),
                   "\U0001f47f `icdcol` \\('invalid_column'\\) was not found as a column in `ph.data`.")

      # valid by columns
      expect_error(death_other_count(ph.data = ph.data_clean, cause = "A00", by = c("invalid_column")),
                   "\U0001f6d1 The following `by` values are not column names in `ph.data`: invalid_column.")

      # valid ypll_age values
      expect_error(death_other_count(ph.data = ph.data_clean, cause = "A00", ypll_age = 0),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")
      expect_error(death_other_count(ph.data = ph.data_clean, cause = "A00", ypll_age = 100),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")
      expect_error(death_other_count(ph.data = ph.data_clean, cause = "A00", ypll_age = "10"),
                   "\U0001f47f `ypll_age` must be an integer between 1 and 99.")

      # valid death_age_col
      expect_error(death_other_count(ph.data = ph.data_clean, cause = "A00", ypll_age = 75, death_age_col = "invalid_column"),
                   "\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
      })

    test_that("Death counts & YPLL counts by cause are accurate ...", {
      expect_equal(dim(other.rads), dim(other.manual)) # table size
      expect_identical(names(other.rads), names(other.manual)) # col names
      expect_identical(sort(unique(other.rads$cause.of.death)), sort(unique(other.manual$cause.of.death))) # COD

      expect_equal(sum(other.rads$deaths), sum(other.manual$deaths)) # cause specific deaths

      expect_equal(sum(other.rads$ypll_65), sum(other.manual$ypll_65)) # cause specific YPLL
    })

    test_that("'cause' argument works correctly ...", {
      expect_identical(
        sort(unique(death_other_count(ph.data = copy(deathDT),
                          cause = 'heart disease',
                          icdcol = "underlying_cod_code",
                          by = c('temperament'),
                          ypll_age = 65,
                          death_age_col = 'age')[]$cause.of.death)),
        c('All causes', 'Heart disease')
        )

        expect_identical(
          sort(unique(death_other_count(ph.data = deathDT,
                                        cause = c('heat', 'stress', 'drug'),
                                        icdcol = "underlying_cod_code",
                                        by = c('temperament'),
                                        ypll_age = 65,
                                        death_age_col = 'age')[]$cause.of.death)),
          c('All causes', 'Drug-induced', 'Drug-overdose', 'Drug_Death', 'HeatStress_Death')
        )
    })

# Check death_injury_matrix_count ----
  # death_injury_matrix_count() create data ----
    deathDT <- rads.data::synthetic_death

  # death_injury_matrix_count() create output ----
    # using the function
    injuries.rads <- death_injury_matrix_count(ph.data = deathDT,
                                               intent = "*",
                                               mechanism = "*",
                                               icdcol = "underlying_cod_code")
    injuries.rads <- injuries.rads[!mechanism %in% c("All transport", "Fire/hot object or substance")] # These are summary categories
    injuries.rads <- injuries.rads[deaths != 0]

    # manually
    cod.of.interest.ref <- rads.data::icd10_death_injury_matrix[, .(underlying_cod_code = icd10, mechanism, intent)]
    injuries.manual <- merge(deathDT[, .(underlying_cod_code, age)], cod.of.interest.ref, by = "underlying_cod_code", all = FALSE)
    injuries.manual[, ypll_65 := fifelse(age < 65, 65 - age, 0)]
    injuries.manual <- injuries.manual[, .(deaths = .N, ypll_65 = sum(ypll_65)), by = c('mechanism', 'intent')]
    setcolorder(injuries.manual, names(injuries.rads))
    injuries.manual <- injuries.manual[!mechanism %in% c("All transport", "Fire/hot object or substance")] # These are summary categories

  # death_injury_matrix_count() tests ----
  test_that("Check for proper triggering of errors ...", {
    expect_error(death_injury_matrix_count(ph.data = "deathDT")) # name of data.table must be unquoted
    expect_error(death_injury_matrix_count(ph.data = deathDTx)) # warning because data.table doesn't exist
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, intent = "z", , icdcol = "underlying_cod_code")) # Should error because none of the intents have 'z'
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, , icdcol = "underlying_cod_code", mechanism = "z")) # Should error because none of the mechanisms have 'z'
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, , icdcol = "underlying_cod_code", intent = 100)) # Should error because intent must be a character
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, , icdcol = "underlying_cod_code", mechanism = 100)) # Should error because mechanism must be a character
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, icdcol = underlying_cod_code)) # Should error because icdcol should be quoted
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, icdcol = "underlying_cod_codex")) # Should error because icdcol does not exist
    expect_error(death_injury_matrix_count(ph.data = injurydata_clean, icdcol = "underlying_cod_code", ypll_age = 65)) # Should error because data lacks dob/dod
    expect_error(death_injury_matrix_count(ph.data = injurydata4, icdcol = "underlying_cod_code", ypll_age = 65)) # Should error because didn't specify age
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "underlying_cod_code", ypll_age = "65")) # Should error because ypll_age is character
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "underlying_cod_code", ypll_age = 65.1)) # Should error because ypll_age is not integer
    expect_error(death_injury_matrix_count(ph.data = injurydata5, icdcol = "underlying_cod_code", by = 'stratum')) # Should error because column is `strata`, not stratum
  })

  test_that("Filtering by intent and mechanism work properly ...", {
    intent.check <- death_injury_matrix_count(ph.data = deathDT,
                                              intent = "suicide",
                                              mechanism = "*",
                                              icdcol = "underlying_cod_code")
    mechanism.check <- death_injury_matrix_count(ph.data = deathDT,
                                                 intent = "*",
                                                 mechanism = "firearm",
                                                 icdcol = "underlying_cod_code")
    double.none <- suppressWarnings(death_injury_matrix_count(ph.data = deathDT,
                                                              intent = "none",
                                                              mechanism = "none",
                                                              icdcol = "underlying_cod_code"))
    expect_equal(unique(intent.check$intent), 'Suicide')
    expect_equal(nrow(mechanism.check), 5) # the '*' gets all five intents
    expect_equal(nrow(double.none), 1) # All injury/Any intent
  })

  test_that("Death counts are accurate ...", {
    expect_equal(sum(injuries.rads[mechanism == "All injury"]$deaths), sum(injuries.manual[mechanism == "All injury"]$deaths)) # summary by intent
    expect_equal(sum(injuries.rads[mechanism != "All injury"]$deaths), sum(injuries.manual[mechanism != "All injury"]$deaths)) # individual mechanisms
    expect_equal(sum(death_injury_matrix_count(ph.data = deathDT, icdcol = "underlying_cod_code", by = 'temperament')[]$deaths),
                 sum(death_injury_matrix_count(ph.data = deathDT, icdcol = "underlying_cod_code")[]$deaths)) # by should not impact total
  })

  test_that("YPLL counts are accurate ...", {
    # compare to manually calculated YPLL_65
    expect_equal(sum(suppressWarnings(death_injury_matrix_count(ph.data = deathDT,
                                               intent = "none",
                                               mechanism = "none",
                                               icdcol = "underlying_cod_code",
                                               ypll_age = 65,
                                               death_age_col = "age"))[]$ypll_65),
                 sum(injuries.manual[mechanism == 'All injury']$ypll_65))

  })

  test_that("Structure of output table is as expected ...", {
    expect_equal(sort(names(injuries.rads)), c("deaths", "intent", "mechanism"))
    expect_equal(sort(names(death_injury_matrix_count(ph.data = deathDT, icdcol = "underlying_cod_code", by = 'temperament'))),
                 sort(c('mechanism', 'intent', 'deaths', 'temperament')))
    expect_equal(sort(unique(death_injury_matrix_count(ph.data = deathDT, icdcol = "underlying_cod_code", by = 'temperament')[]$temperament)),
                 sort(c('Active', 'Calm', 'Moderate')))
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

# Check life_table (basic) ----
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

  test_that('Confirm that by argument works when specified...',{
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
                                                by = c('shape', 'color'),
                                                ci = 0.95))
      expect_equal(nrow(test_groups), 76) # 76 because 4 stratum and 19 age groups

      expect_no_error(test_groups2 <- life_table(ph.data = dt_groups2,
                                                 myages = 'ages',
                                                 mydeaths = 'deaths',
                                                 mypops = 'pop',
                                                 myprops = 'fraction',
                                                 by = c('shape', 'color'),
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

# Check life_table (with by) ----
  # life_table() create data ----
    dt <- data.table(
      sex = "Male",
      city = "Gotham",
      ages = c("0-1", "1-5", "5-10", "10-15", "15-18", "18-20", "20-25", "25-30",
               "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65",
               "65-70", "70-75", "75-80", "80-85", "85+"),
      deaths = c(10, 10, 10, 0, 10, 10, 30, 10, 100, 90, 120, 140, 190, 190,
                 420, 580, 560, 760, 830, 1960),
      fraction = c(0, 0.352877, 0.621634, 0, 0.709589, 0.037443, 0.411568,
                   0.822831, 0.308035, 0.330746, 0.448096, 0.422047, 0.468697,
                   0.364526, 0.43501, 0.435655, 0.467277, 0.39805, 0.344557, 0.376492),
      pop = c(6990, 29870, 41350, 41560, 25360, 15100, 33890, 44780, 57660,
              59310, 56660, 45910, 45870, 42060, 43130, 39220, 30090, 19660,
              11030, 10320)
    )

  # life_table() create output ----
    test1 <- life_table(ph.data = copy(dt)[], by=c('sex', 'city'))
    test2 <- suppressWarnings(life_table(ph.data = copy(dt)[ages %in% c('80-85'), deaths := 0], by=c('sex', 'city')))
    test3 <- suppressWarnings(life_table(ph.data = copy(dt)[ages %in% c('85+'), deaths := 0], by=c('sex', 'city')))
    test4 <- suppressWarnings(life_table(ph.data = copy(dt)[as.numeric(substr(ages, 1, 2)) <=60 , deaths := 0], by=c('sex', 'city')))

  # life_table() tests ----
    expect_no_warning(life_table(ph.data = copy(dt)[], by=c('sex', 'city')))
    expect_no_warning(life_table(ph.data = copy(dt)[ages %in% c('80-85'), deaths := 0], by=c('sex', 'city')))
    expect_warning(life_table(ph.data = copy(dt)[ages %in% c('85+'), deaths := 0], by=c('sex', 'city')),
                   "function has provided modeled `mx` values")
    expect_warning(life_table(ph.data = copy(dt)[as.numeric(substr(ages, 1, 2)) <=60 , deaths := 0], by=c('sex', 'city')),
                   "Small population issue for sex = Male, city = Gotham")
    expect_error(life_table(ph.data = copy(dt)[ages %in% c('80-85', '85+'), deaths := 0], by=c('sex', 'city')),
                 "This almost certainly means that your population is too small for life table estimation")

    # when set deaths to zero for second oldest group, expect life expectancy to bump up
    expect_gt(test2[1]$ex, test1[1]$ex)

    # when set deaths to zero for oldest group, also expect life expectancy to bump up
    expect_gt(test3[1]$ex, test1[1]$ex)

    # When drop all deaths for those under 60, expect a large jump in life expectancy
    expect_gt(test4[1]$ex - test1[1]$ex, test1[2]$ex - test1[1]$ex)
    expect_gt(test4[1]$ex - test1[1]$ex, test1[3]$ex - test1[1]$ex)

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
    mylifetable <- suppressWarnings(life_table(ph.data = dt, by = 'shape'))
    setorder(mylifetable, ages, shape)

  # Tests ----
    test_that("Check messages, proper filling of missing values, and LE0 estimates ...", {
      expect_warning(life_table(ph.data = dt, by = 'shape'), "Zero deaths detected")
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
  ltp_output_group <- life_table_prep(ph.data = ltp, by = c('year', 'race_eth'))

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

  test_that("Check that by command works as expected ...", {
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
    ltp_output_group_alt <- life_table_prep(ph.data = ltp, by = c('year', 'race_eth'))

    # test
    expect_identical(ltp_output_group_alt[race_eth == 'Hispanic' & ages == '85+']$deaths, 0L)
  })

