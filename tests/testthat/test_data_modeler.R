

test_that("data_modeler creates matching types", {
  observations <- 1000
  DTTest <- data.table(
    id = 1:observations,
    chi_geo_kc = sample(c('King County',NA_character_), observations, replace = T),
    chi_race_7 = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
    chi_sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
    chi_geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
    indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
    indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
    indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)),
    dates_baser = as.Date(sample(c("2025-01-01", "2025-06-01","2025-12-01"), observations, replace = T, prob = c(.33, .33, .33))),
    dates_posix14 = as.POSIXct(sample(c("2025-01-15 00:00:00 UTC","2025-02-15 00:00:00 UTC","2025-03-15 00:00:00 UTC","2025-04-15 00:00:00 UTC","2025-05-15 00:00:00 UTC","2025-06-15 00:00:00 UTC","2025-07-15 00:00:00 UTC","2025-08-15 00:00:00 UTC","2025-09-15 00:00:00 UTC","2025-10-15 00:00:00 UTC","2025-11-15 00:00:00 UTC","2025-12-15 00:00:00 UTC","2026-01-15 00:00:00 UTC","2026-02-15 00:00:00 UTC"), observations, replace = TRUE, prob = c(.05,.05,.05,.05,.05,.10,.10,.10,.10,.10,.10,.15,.05,.05)), tz = "UTC"))

  DTResult <- data_modeler(ph.data = DTTest, number_of_observations = 100, comments = T, return_code = F, print_code = T)

  data_types_test <- sapply(DTTest, class)
  data_types_result <- sapply(DTResult, class)

  #all types match
  expect_equal(all(unlist(data_types_test) == unlist(data_types_result)), TRUE)

})

test_that("data_modeler handles apostrophies", {
  observations <- 1000
  DTTest <- data.table(
    id = 1:observations,
    indicator3 = sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T),
    indicator4 = as.factor(sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T)))

  # test if executes apostrophes
  expect_no_error(data_modeler(ph.data = DTTest, number_of_observations = 100, comments = T, return_code = F, print_code = T))


  # test if capture quotation marks

  # test if provides accurate error if unable to match (contains both quote and apostrophe

})
