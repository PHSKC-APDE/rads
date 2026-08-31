test_that("data_modeler creates matching types", {
  observations <- 1000
  DTTest <- data.table::data.table(
    id = 1:observations,
    county = sample(c('County1', 'County2'), observations, replace = TRUE),
    race = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
    sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
    geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
    age = as.integer(pmin(pmax(rnorm(observations, mean = 50, sd = 15),0),100)),
    inches = as.numeric(pmin(pmax(rnorm(observations, mean = 68, sd = 10),30),90)),
    indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
    indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
    indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)),
    indicator4 = as.numeric(sample(1:100, observations, replace = T)),
    indicator5 = sample(1:3, observations, replace = T),
    indicator6 = as.double(sample(1:100, observations, replace = T)),
    indicator7 = as.numeric(sample(1:10, observations, replace = T)),
    dates_baser = as.Date(sample(c("2025-01-01", "2025-06-01","2025-12-01"), observations, replace = T, prob = c(.33, .33, .33))),
    dates_posix14 = as.POSIXct(sample(c("2025-01-15 00:00:00 UTC","2025-02-15 00:00:00 UTC","2025-03-15 00:00:00 UTC","2025-04-15 00:00:00 UTC","2025-05-15 00:00:00 UTC","2025-06-15 00:00:00 UTC","2025-07-15 00:00:00 UTC","2025-08-15 00:00:00 UTC","2025-09-15 00:00:00 UTC","2025-10-15 00:00:00 UTC","2025-11-15 00:00:00 UTC","2025-12-15 00:00:00 UTC","2026-01-15 00:00:00 UTC","2026-02-15 00:00:00 UTC"), observations, replace = TRUE, prob = c(.05,.05,.05,.05,.05,.10,.10,.10,.10,.10,.10,.15,.05,.05)), tz = "UTC"))

  DTResult <- data_modeler(ph.data = DTTest, number_of_observations = 1000, comments = T, return_code = F, print_code = F)

  DFResult <- data_modeler(ph.data = data.table::as.data.table(DTTest), number_of_observations = 1000, comments = T, return_code = F, print_code = F)

  data_types_test <- sapply(DFTest, class)
  data_types_result <- sapply(DFResult, class)


  data_types_test <- sapply(DTTest, class)
  data_types_result <- sapply(DTResult, class)

  #all types match
  expect_equal(all(unlist(data_types_test) == unlist(data_types_result)), TRUE)

})

test_that("data_modeler returns data frame when given a data frame", {
  observations <- 1000
  DTTest <- data.table::data.table(
    id = 1:observations)
  DFTest <- as.data.frame(DTTest)
  DFResult <- data_modeler(ph.data = DFTest, number_of_observations = 1000, comments = T, return_code = F, print_code = F)
  expect_true(inherits(DFResult, "data.frame"))

})

test_that("data_modeler handles apostrophies", {
  observations <- 1000
  DTTest <- data.table(
    id = 1:observations,
    indicator3 = sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T),
    indicator4 = as.factor(sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T)))

  # test if handles apostrophes
  expect_no_error(data_modeler(ph.data = DTTest, number_of_observations = 100, comments = T, return_code = F, print_code = F))

})

test_that("data_modeler handles single column DT objects", {
  observations <- 1000

  DTTest.1.cat <- data.table(
    race = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA))
  )

  DTTest.1.num <- data.table(
    `constant.numi` = as.numeric(sample(c(1), 1000, replace = TRUE, prob = c(1)))
  )

  DTResult.1.cat <- data_modeler(ph.data = DTTest.1.cat, number_of_observations = 1000, comments = T, return_code = F, print_code = F)


  DTResult.1.num <- data_modeler(ph.data = DTTest.1.num, number_of_observations = 1000, comments = T, return_code = F, print_code = F)

  data_types_test.1.cat <- sapply(DTTest.1.cat, class)
  data_types_result.1.cat <- sapply(DTResult.1.cat, class)

  expect_equal(all(unlist(data_types_test.1.cat) == unlist(data_types_result.1.cat)), TRUE)

  data_types_test.1.num <- sapply(DTTest.1.num, class)
  data_types_result.1.num <- sapply(DTResult.1.num, class)

  expect_equal(all(unlist(data_types_test.1.num) == unlist(data_types_result.1.num)), TRUE)
})

test_that("data_modeler characters don't generate unneeded warnings", {
  observations <- 1000

  DT.test.data <- data.table::data.table(
    `status_a` = as.factor(sample(c(NA, "bunny", "horse"), observations, replace = TRUE, prob = c(0.10, .40, 0.50))), # as factor
    `status_b` = as.character(sample(c(NA, 'up', 'down'), observations, replace = TRUE, prob = c(0.20, 0.30, 0.50))), # as categorical character (non factor)
    `status_c` = as.character(round(rnorm(observations, sd = 10, mean = 20), 6)), #as numeric looking character
    `status_d` = replicate(observations, sapply(sample(1:7,1), function(x) {paste0(sample(c("Lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscin"), x, replace = T), collapse = " ")})) # as character, free text
  )

  RANDOMROWS <- sample(1:observations, observations*.10)

  DT.test.data[RANDOMROWS, `:=`(status_c = NA, status_d = NA)]

  expect_no_warning(data_modeler( ph.data = DT.test.data,
                                  number_of_observations = 1000,
                                  comments = T,
                                  return_code = F,
                                  print_code = F))
})


test_that("data_modeler numbers don't generate unneeded warnings", {
  observations <- 1000

  DT.test.data <- data.table::data.table(
    `createID` = 1:observations, # as an identifying (all unique) integer
    `status_a` = as.integer(sample(c(NA, '0', '1'), observations, replace = TRUE, prob = c(0.05, 0.05, 0.90))), # as categorical integer (non factor)
    `status_b` = as.integer(rnorm(observations, sd = 50)), # as continuous integer with uniform distribution
    `status_c` = as.double(round((rnorm(observations, sd = 50) *0.1), 5)), # as continuous double with uniform distribution
    `status_d` = as.numeric(sample(c(NA, '.405', '.21'), observations, replace = TRUE, prob = c(0.05, 0.05, 0.90))) # as categorical numeric (non factor)
  )

  RANDOMROWS <- sample(1:observations, observations*.10)

  DT.test.data[RANDOMROWS, `:=`(status_b = NA, status_c = NA, status_d = NA)]

  expect_no_warning(data_modeler( ph.data = DT.test.data,
                                  number_of_observations = observations,
                                  comments = T,
                                  return_code = F,
                                  print_code = F))
})

test_that("data_modeler dates don't generate unneeded warnings", {
  observations <- 1000

  DT.test.data <- data.table::data.table(
    `status_a` = as.Date(sample(c(NA, "2010-01-01", "2020-01-01"), observations, replace = TRUE, prob = c(0.10, .40, 0.50))), #as Date (with original probability)
    `status_b` = as.Date(paste0("2024-01-",round(rnorm(observations, sd = 4,mean =  15) ) )) # as Date (with uniform distribution by day)
  )

  RANDOMROWS <- sample(1:observations, observations*.10)

  DT.test.data[RANDOMROWS, `:=`( status_b = NA)]


  expect_no_warning(data_modeler( ph.data = DT.test.data,
                                  number_of_observations = 1000,
                                  comments = T,
                                  return_code = F,
                                  print_code = F))
})
