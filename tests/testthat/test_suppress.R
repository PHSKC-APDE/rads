library('data.table')
library('testthat')

# create test data ----
set.seed(98104)
dt <- suppressWarnings(data.table::data.table(chi_year = 2022,
                             indicator = "home ownership",
                             team = sample(as.vector(outer(letters, 1:15, paste0)), rep = T),
                             color = c("red", "blue", "yellow", "green"),
                             numerator = sample(1:200, 1000, rep = TRUE)))
dt[, denominator := sample(500:1000, 1000, rep = TRUE)]
dt[, mean := numerator / denominator]
dt[, se := sqrt(mean/100)] # not a real formula!
dt[, lower_bound := mean - (1.96 * se)]
dt[, upper_bound := mean + (1.96 * se)]
dt[, rse := 100*se / mean]
setorder(dt, indicator, team, color, numerator)
dt[, counter := 1:.N, c("indicator", "team", "color")]
dt <- dt[counter == 1][, counter := NULL]

# test defaults ----
dt1 <- suppress(dt)
test_that('Check that defaults work as expected',{
  expect_equal( nrow(dt1[suppression=="^"]), nrow(dt[numerator <= 9]))
  expect_equal( nrow(dt1[suppression=="^"]), nrow(dt1[is.na(mean)]))
  expect_equal( nrow(dt1[suppression=="^"]), nrow(dt1[is.na(se)]))
  expect_equal( nrow(dt1[suppression=="^"]), nrow(dt1[is.na(lower_bound)]))
  expect_equal( nrow(dt1[suppression=="^"]), nrow(dt1[is.na(rse)]))
  expect_equal( nrow(dt1[caution=="!"]), nrow(dt[numerator > 9 & rse >=30]))
})



# test suppression range ----
dt2 <- suppress(dt, suppress_range = c(0,10), secondary = F)
test_that('Check that the suppression_range argument works',{
  expect_equal( nrow(dt2[suppression=="^"]), nrow(dt[numerator <= 10]))
  expect_equal( nrow(dt2[suppression=="^"]), nrow(dt2[is.na(mean)]))
  expect_equal( nrow(dt2[suppression=="^"]), nrow(dt2[is.na(se)]))
  expect_equal( nrow(dt2[suppression=="^"]), nrow(dt2[is.na(lower_bound)]))
  expect_equal( nrow(dt2[suppression=="^"]), nrow(dt2[is.na(rse)]))
  expect_equal( nrow(dt2[caution=="!"]), nrow(dt[numerator > 10 & rse >=30]))
})



# test secondary suppression ----
dt3 <- suppress(dt, suppress_range = c(0,10),
                secondary = T,
                secondary_ids = c("indicator", "team"))
  #ugly manual method to apply secondary suppression for comparison
    sec.suppress3 <- copy(dt2) # build off results from initial / primary suppression
    sec.suppress3[, max.grp.rows := .N, .(indicator, team)] # num of rows per set of secondary_ids
    sec.suppress3[, group := .GRP, by = .(indicator, team)] # create group id for each set of secondary_ids
    supp.ids <- unique(sec.suppress3[suppression=="^"]$group) # get group ids where there was initial suppression
    sec.suppress3[, suppressed.group := F]
    sec.suppress3[group %in% supp.ids, suppressed.group := T] # identify groups with initial suppression in table
    sec.suppress3[group %in% supp.ids & is.na(suppression), unsuppressed := .N, .(indicator, team)] # rows unsuppressed per group
    suppressWarnings(sec.suppress3[, unsuppressed := max(unsuppressed, na.rm = T), .(indicator, team)]) # fill in NA for rows unsuppressed
    sec.suppress3[is.na(suppression) & unsuppressed == max.grp.rows - 1, secondary.suppression := T] # identify groups that need secondary suppression (groups with exactly one suppressed row)
    setorder(sec.suppress3, group, numerator, na.last = T) # sort from smallest to largest numerator by group
    sec.suppress3[secondary.suppression == T, order := 1:.N, group] # identify 1st row (smallest numerator) of each group needing secondary suppression
    sec.suppress3[order==1, suppression := "^"] # mark the specific rows to have secondary suppression
    sec.suppress3[suppression == "^", c("numerator", "denominator", "mean", "se", "lower_bound", "upper_bound", "rse", "caution") := NA]
    sec.suppress3[, c("max.grp.rows", "group", "suppressed.group", "unsuppressed", "secondary.suppression", "order") := NULL]

test_that('Check that secondary suppression works',{
  expect_equal( nrow(dt3[suppression=="^"]), nrow(sec.suppress3[suppression=="^"]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(mean)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(se)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(lower_bound)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(rse)]))
})

# test secondary suppression with secondary_exclude ----
dt4 <- suppress(dt, suppress_range = c(0,10),
                secondary = T,
                secondary_ids = c("indicator", "team"),
                secondary_exclude = !team %in% c('a10', 'a11'))

  #ugly manual method to apply secondary suppression for testing
    exclusion4 <- copy(dt2)[team %in% c('a10', 'a11')] # partition off part excluded from secondary suppression
    sec.suppress4 <- copy(dt2)[!team %in% c('a10', 'a11')] # build off results from initial / primary suppression
    sec.suppress4[, max.grp.rows := .N, .(indicator, team)] # num of rows per set of secondary_ids
    sec.suppress4[, group := .GRP, by = .(indicator, team)] # create group id for each set of secondary_ids
    supp.ids <- unique(sec.suppress4[suppression=="^"]$group) # get group ids where there was initial suppression
    sec.suppress4[, suppressed.group := F]
    sec.suppress4[group %in% supp.ids, suppressed.group := T] # identify groups with initial suppression in table
    sec.suppress4[group %in% supp.ids & is.na(suppression), unsuppressed := .N, .(indicator, team)] # rows unsuppressed per group
    suppressWarnings(sec.suppress4[, unsuppressed := max(unsuppressed, na.rm = T), .(indicator, team)]) # fill in NA for rows unsuppressed
    sec.suppress4[is.na(suppression) & unsuppressed == max.grp.rows - 1, secondary.suppression := T] # identify groups that need secondary suppression (groups with exactly one suppressed row)
    setorder(sec.suppress4, group, numerator, na.last = T) # sort from smallest to largest numerator by group
    sec.suppress4[secondary.suppression == T, order := 1:.N, group] # identify 1st row (smallest numerator) of each group needing secondary suppression
    sec.suppress4[order==1, suppression := "^"] # mark the specific rows to have secondary suppression
    sec.suppress4[suppression == "^", c("numerator", "denominator", "mean", "se", "lower_bound", "upper_bound", "rse", "caution") := NA]
    sec.suppress4[, c("max.grp.rows", "group", "suppressed.group", "unsuppressed", "secondary.suppression", "order") := NULL]
    sec.suppress4 <- rbind(sec.suppress4, exclusion4)

test_that('Check that secondary suppression works',{
  expect_equal( nrow(dt4[suppression=="^"]), nrow(sec.suppress4[suppression=="^"]))
  expect_equal( nrow(dt4[suppression=="^"]), nrow(dt4[is.na(mean)]))
  expect_equal( nrow(dt4[suppression=="^"]), nrow(dt4[is.na(se)]))
  expect_equal( nrow(dt4[suppression=="^"]), nrow(dt4[is.na(lower_bound)]))
  expect_equal( nrow(dt4[suppression=="^"]), nrow(dt4[is.na(rse)]))
})



# test flag_only ----
dt5 <- suppress(dt, flag_only = T)
test_that('Check that flag_only works',{
  expect_equal( nrow(dt5[suppression=="^"]), nrow(dt[numerator <= 9]))
  expect_equal( 0, nrow(dt5[is.na(mean)]))
  expect_equal( 0, nrow(dt5[is.na(se)]))
  expect_equal( 0, nrow(dt5[is.na(lower_bound)]))
  expect_equal( 0, nrow(dt5[is.na(rse)]))
  expect_equal( nrow(dt5[caution=="!"]), nrow(dt[rse >=30]))
})



# test secondary_exclude when character and unquoted expression ----

test_that('Check that the same results are returned whether or not quoted',{
  expect_warning(dt6 <- suppress(dt, secondary_exclude = "team %like% '^a|^b|^c|^d'"))
  dt7 <- suppress(dt, secondary_exclude = team %like% '^a|^b|^c|^d')
  expect_identical( dt6, dt7)
})

