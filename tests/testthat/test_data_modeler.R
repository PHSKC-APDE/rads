

test_that("data_modeler creates correct types", {
  observations <- 1000
  DTTest <- data.table(
    id = 1:observations,
    chi_geo_kc = sample(c('King County',NA_character_), observations, replace = T),
    chi_race_7 = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
    chi_sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
    chi_geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
    indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
    indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
    indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)))

  DTResult <- data_modeler(ph.data = DTTest, number_of_observations = 100, comments = T, return_code = F, print_code = T)

  data_types_test <- sapply(DTTest, class)
  data_types_result <- sapply(DTResult, class)

  #all types match
  expect_equal(all(data_types_test == data_types_result), TRUE)

})

testthat("data_modeler handles apostrophies", {
  observations <- 1000
  DTTest <- data.table(
    id = 1:observations,
    indicator3 = sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T),
    indicator4 = as.factor(sample(c("jeremy's long sentence", "They're not having it!"), observations, replace = T)))

  # test if executes apostrophes
  expect_no_error(data_modeler(ph.data = DTTest, number_of_observations = 100, comments = T, return_code = F, print_code = T))


  # test if capture quotation marks

  #test if provides accurate error if unable to match (contains both quote and apostrophe


})
