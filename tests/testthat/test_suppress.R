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
dt2 <- suppress(dt, suppress_range = c(0,10))
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
  #ugly manual method to apply secondary suppression for testing
    sec.suppress3 <- copy(dt2)
    max.grp.rows <- max(sec.suppress3[, rowct := .N, .(indicator, team)]$rowct)
    sec.suppress3[, group := .GRP, by = c("indicator", "team")]
    sec.suppress3 <- sec.suppress3[group %in% sec.suppress3[suppression=="^"]$group] # id groups with suppression
    sec.suppress3 <- sec.suppress3[is.na(suppression)]
    sec.suppress3 <- sec.suppress3[, rowct := .N, .(indicator, team)]
    sec.suppress3 <- sec.suppress3[rowct == max.grp.rows - 1]
    setorder(sec.suppress3, group, numerator)
    sec.suppress3[, order := 1:.N, group]
    sec.suppress3[order==1, suppression := "^"][, c("rowct", "group", "order") := NULL]
    sec.suppress3 <- rbind(dt2[suppression=="^"], sec.suppress3[suppression=="^"])

test_that('Check that secondary suppression works',{
  expect_equal( nrow(dt3[suppression=="^"]), nrow(sec.suppress3[suppression=="^"]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(mean)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(se)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(lower_bound)]))
  expect_equal( nrow(dt3[suppression=="^"]), nrow(dt3[is.na(rse)]))
})

# test secondary suppression with secondary_where ----
dt4 <- suppress(dt, suppress_range = c(0,10),
                secondary = T,
                secondary_ids = c("indicator", "team"),
                secondary_where = "!team %in% c('a10', 'a11')")

#ugly manual method to apply secondary suppression for testing
  sec.suppress4 <- copy(dt2[!team %in% c('a10', 'a11')])
  max.grp.rows <- max(sec.suppress4[, rowct := .N, .(indicator, team)]$rowct)
  sec.suppress4[, group := .GRP, by = c("indicator", "team")]
  sec.suppress4 <- sec.suppress4[group %in% sec.suppress4[suppression=="^"]$group] # id groups with suppression
  sec.suppress4 <- sec.suppress4[is.na(suppression)]
  sec.suppress4 <- sec.suppress4[, rowct := .N, .(indicator, team)]
  sec.suppress4 <- sec.suppress4[rowct == max.grp.rows - 1]
  setorder(sec.suppress4, group, numerator)
  sec.suppress4[, order := 1:.N, group]
  sec.suppress4[order==1, suppression := "^"][, c("rowct", "group", "order") := NULL]
  sec.suppress4 <- rbind(dt2[suppression=="^"], sec.suppress4[suppression=="^"])

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



# test secondary_where when character and unquoted expression ----
dt6 <- suppress(dt, secondary_where = "team %like% '^a|^b|^c|^d'")
dt7 <- suppress(dt, secondary_where = team %like% '^a|^b|^c|^d')
test_that('Check that the same results are returned whether or not quoted',{
  expect_identical( dt6, dt7)
})

