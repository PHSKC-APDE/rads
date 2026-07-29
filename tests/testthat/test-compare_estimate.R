library('testthat')
library('rads')
library('data.table')
library('dtsurvey')

# create test data ----
set.seed(98104)
dt <- data.table::data.table(
  chi_year = rep(2008:2018, 2000),
  chi_sex = factor(sample(c("Male", "Female"), 22000, rep = TRUE, prob = c(0.5, 0.5))),
  chi_age = round(rnorm(22000, 23, 2.5), 0),
  fetal_pres = factor(sample(c("Breech", "Cephalic", "Other", NA), 22000, rep = TRUE, prob = c(0.04, 0.945, 0.01, 0.005))),
  kotelchuck = sample(c(0, 1, NA), 22000, rep = TRUE, prob = c(0.26, 0.64, 0.10)),
  bw_grams = round(rnorm(22000, 3343, 576), 0)
)
dt[fetal_pres == "Breech" & chi_year == 2018, bw_grams := bw_grams * .75 ]
dt[fetal_pres != "Breech" & chi_year == 2018, bw_grams := bw_grams * 1.75 ]
dt = dtadmin(dt)

# create test estimates ----
dtest <- suppressWarnings(calc(dt, what = c("kotelchuck", "bw_grams", "chi_age", "chi_sex"), by = c("fetal_pres", "chi_year")))

# test validation checks ----
test_that('Check that errors are flagged when fails validation',{
  expect_error(compare_estimate(mydt = matrix(1:10))) # mydt needs to be data.frame | data.table
  expect_error(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "cat"))) # non-existent id_vars
  expect_error(compare_estimate(mydt = dtest, key_where = is.na(chi_year))) # filter out all data
  expect_error(compare_estimate(mydt = dtest, new_col = NULL)) # need a new_col
  expect_error(compare_estimate(mydt = dtest, new_col = "")) # need a new_col
  expect_error(compare_estimate(mydt = dtest, new_col = NA)) # need a new_col
  expect_error(compare_estimate(mydt = dtest, new_col = "fetal_pres")) # new_col cannot exist
  expect_error(compare_estimate(mydt = dtest, tidy = "TRUE")) # tidy must be a logical
})

# test key_where ----
test_that('Check that key_where works quoted and unquoted', {
  expect_equal(suppressWarnings(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = 'chi_year == 2017')),
               compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017 ))
})

test_that('Check that key_where is actually selecting the comparator / referent', {
  expect_equal(nrow(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, tidy = F)[mean == comp_est]),
               nrow(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, tidy = F)[chi_year == 2017]))
  expect_equal( unique(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, tidy = F)[]$comp_est),
                unique(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, tidy = F)[chi_year==2017]$mean))
})

# test new_col ----
test_that('Check that new_col allows changing of new output columns', {
  testthat::expect_true("comp" %in% names(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017)[]))
  testthat::expect_true("comp_sig" %in% names(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017)[]))
  testthat::expect_true("xkct" %in% names(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, new_col = "xkct")[]))
  testthat::expect_true("xkct_sig" %in% names(compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2017, new_col = "xkct")[]))
})

# test comparison results ----
dtcomp <- compare_estimate(mydt = dtest, id_vars = c("variable", "level", "fetal_pres"), key_where = chi_year == 2018)[]
test_that('Check that comparisons and significance columns are correct', {
  expect_equal(1, nrow(dtcomp[fetal_pres == "Breech" & variable == "bw_grams" & comp=="no different"]))
  expect_equal(1, nrow(dtcomp[fetal_pres == "Breech" & variable == "bw_grams" & is.na(comp_sig)]))
  expect_equal(10, nrow(dtcomp[fetal_pres == "Breech" & variable == "bw_grams" & comp=="higher"]))
  expect_equal(10, nrow(dtcomp[fetal_pres == "Breech" & variable == "bw_grams" & comp_sig=="*"]))
  expect_equal(20, nrow(dtcomp[fetal_pres != "Breech" & variable == "bw_grams" & comp=="lower"]))
  expect_equal(20, nrow(dtcomp[fetal_pres != "Breech" & variable == "bw_grams" & comp_sig=="*"]))
})



