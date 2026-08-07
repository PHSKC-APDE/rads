library(data.table)

# adjust_direct() ----
test_that("adjust_direct: input validation errors", {
  expect_error(
    adjust_direct(count = c(1, 2), pop = c(100), stdpop = c(50, 60)),
    "length.*must be equal"
  )

  expect_error(
    adjust_direct(count = c(-1, 2), pop = c(100, 200), stdpop = c(50, 60)),
    "must not contain negative values"
  )

  expect_error(
    adjust_direct(count = c(1, 2), pop = c(100, 200), stdpop = c(50, 60), conf.level = 1),
    "between 0 and 1"
  )
})

test_that("adjust_direct: stdpop = 0 generates warning", {
  expect_warning(
    adjust_direct(count = c(1, 2), pop = c(100, 200), stdpop = c(0, 60)),
    "At least one stratum.*population of 0"
  )
})

test_that('adjust_direct calculations',{

  temp.direct <- adjust_direct(count = c(11, 9), pop = c(500, 500), stdpop = c(640, 720), per = 100, conf.level = 0.95)

  expect_equal(20, temp.direct[["count"]] )

  expect_equal(2, temp.direct[["crude.rate"]] )

  expect_equal(1.2217, round(temp.direct[["crude.lci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(3.0888, round(temp.direct[["crude.uci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(1.9882, round(temp.direct[["adj.rate"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(1.2133, round(temp.direct[["adj.lci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(3.0820, round(temp.direct[["adj.uci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

})

test_that("adjust_direct: count = 0 and pop > 0", {
  res <- adjust_direct(count = c(0, 0), pop = c(100, 200), stdpop = c(1, 1))
  expect_equal(res[["crude.rate"]], 0)
  expect_equal(res[["adj.rate"]], 0)
  expect_equal(res[["crude.lci"]], 0)
})

test_that("adjust_direct: pop = 0 and count = 0 (no warning expected)", {
  expect_silent({
    res <- adjust_direct(count = c(0, 0), pop = c(0, 0), stdpop = c(1, 1))
  })
  expect_equal(res[["crude.rate"]], 0)
  expect_equal(res[["adj.rate"]], 0)
})

test_that("adjust_direct: pop = 0 and count > 0 with repeatable events (warning and NA)", {
  expect_error(
    res <- adjust_direct(
      count = c(5, 10),
      pop = c(0, 0),
      stdpop = c(1, 1),
      event_type = "repeatable"
    ), "Cannot calculate rates when all populations are zero"
  )
})

test_that("adjust_direct: pop < count with unique events (capped at 100%)", {
  res <- adjust_direct(
    count = c(25, 30),
    pop = c(20, 25),
    stdpop = c(500, 900),
    per = 100,
    event_type = "unique"
  )
  expect_true(res[["crude.rate"]] == 100)
  expect_true(res[["adj.rate"]] == 100)
})

test_that("adjust_direct: pop < count with repeatable events (rates > 100% allowed)", {
  res <- adjust_direct(
    count = c(25, 30),
    pop = c(20, 25),
    stdpop = c(500, 900),
    per = 100,
    event_type = "repeatable"
  )
  expect_true(res[["crude.rate"]] > 100)
  expect_true(res[["adj.rate"]] > 100)
})

test_that("adjust_direct: very small population edge case", {
  res <- adjust_direct(
    count = c(1),
    pop = c(1),
    stdpop = c(1),
    per = 1000
  )
  expect_equal(res[["crude.rate"]], 1000)
  expect_true(res[["crude.uci"]] > res[["crude.lci"]])
})

# age_standardize() ----
test_that('valid output',{
  temp.dt1 <- data.table(age = c(50:60), count = c(25:35), pop = c(seq(1000, 800, -20)) )
  temp.agestd1 <- suppressWarnings(age_standardize(ph.data = temp.dt1, ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T,
                                                   my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95))

  expect_equal(sum(temp.dt1$count), temp.agestd1[["count"]])

  expect_equal(round(1000*sum(temp.dt1$count) / sum(temp.dt1$pop), 4), temp.agestd1[["crude.rate"]])

  expect_equal(29.8335, temp.agestd1[["crude.lci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(37.1309, temp.agestd1[["crude.uci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.1021, temp.agestd1[["adj.rate"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(30.6216, temp.agestd1[["adj.lci"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(40.2520, temp.agestd1[["adj.uci"]] ) # checked vis-à-vis epitools::ageadjust.direct


  temp.dt2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
                         count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
                         stdpop = rep(1000, 22))

  expect_message(age_standardize(ph.data = temp.dt2, ref.popname = "none", collapse = F, my.count = "count",
                                 my.pop = "pop", per = 1000, conf.level = 0.95, by = "sex", diagnostic_report = TRUE),
                 'Returning diagnostic report')

  diagreport <- suppressMessages(age_standardize(ph.data = temp.dt2, ref.popname = "none", collapse = F, my.count = "count",
                                                 my.pop = "pop", per = 1000, conf.level = 0.95, by = "sex", diagnostic_report = TRUE))
  expect_identical(unique(diagreport$missing), '0-49, 61-100')
  expect_identical(sort(unique(diagreport$sex)), c('F', 'M'))

  temp.agestd2 <- suppressWarnings(age_standardize(ph.data = temp.dt2, ref.popname = "none", collapse = F, my.count = "count",
                                                   my.pop = "pop", per = 1000, conf.level = 0.95, by = "sex"))

  expect_equal(sum(temp.dt2[sex == "M"]$count) , temp.agestd2[sex == "M"]$count)
  expect_equal(sum(temp.dt2[sex == "F"]$count) , temp.agestd2[sex == "F"]$count)

  expect_equal(round(1000*sum(temp.dt2[sex == "M"]$count) / sum(temp.dt2[sex == "M"]$pop), 4) , temp.agestd2[sex == "M"]$crude.rate)
  expect_equal(round(1000*sum(temp.dt2[sex == "F"]$count) / sum(temp.dt2[sex == "F"]$pop), 4) , temp.agestd2[sex == "F"]$crude.rate)

  expect_equal(28.2633 , temp.agestd2[sex == "M"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(26.4730 , temp.agestd2[sex == "F"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.1766 , temp.agestd2[sex == "M"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(32.8298 , temp.agestd2[sex == "F"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(31.7250 , temp.agestd2[sex == "M"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(29.6415 , temp.agestd2[sex == "F"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(28.3923 , temp.agestd2[sex == "M"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(26.5772 , temp.agestd2[sex == "F"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(35.3468 , temp.agestd2[sex == "M"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(32.9665 , temp.agestd2[sex == "F"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct
})

test_that('errors & warnings',{
  set.seed(98104)
  temp.dt3 <- data.table(age = c(0:100), count = sample(1000:4000, size = 101), pop = sample(10000:40000, size = 101) )

  expect_silent(age_standardize(temp.dt3, my.count = "count", my.pop = "pop")) # no error, no warning

  expect_error(age_standardize(copy(temp.dt3)[1, age := NA], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[1, age := 103], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, age := -1], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[!2, ], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[!1], my.count = "count", my.pop = "pop"))

  expect_warning(age_standardize(copy(temp.dt3)[1, count := NA], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, count := -1], my.count = "count", my.pop = "pop"))

  expect_error(age_standardize(copy(temp.dt3)[1, pop:= NA], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, pop := -1], my.count = "count", my.pop = "pop"))

  expect_warning(age_standardize(copy(temp.dt3)[1, count := pop + 1], my.count = "count", my.pop = "pop"))

  expect_error(age_standardize(copy(temp.dt3)[, agecat := 'myagegroup'], my.count = "count", my.pop = "pop"),
               "Both 'age' and 'agecat' columns are present, but only one is needed")

  expect_error(age_standardize(copy(temp.dt3)[, age := NULL], my.count = "count", my.pop = "pop"),
               "Neither 'age' nor 'agecat' columns are present.")

  expect_error(age_standardize(copy(temp.dt3)[, age := age + 0.1], my.count = "count", my.pop = "pop"),
               "The 'age' column must be numeric")

  expect_error(age_standardize(copy(temp.dt3)[, age := NULL][, agecat := 10], my.count = "count", my.pop = "pop"),
               "The 'agecat' column is neither character nor factor")

  # detailed warnings when missing ages in specific groups defined by by
  set.seed(98104)
  tempy <- data.table(
    gender = sample(c('F', 'M'), 20000, replace = T),
    height = sample(c('Short', 'Tall'), 20000, replace = T),
    weight = sample(c('Heavy', 'Light'), 20000, replace = T),
    age = sample(0:100, 20000, replace = T),
    disease = sample(0:1, 20000, replace = T))
  tempy <- tempy[, .(pop = .N, disease = sum(disease)), .(gender, height, weight, age)]

  tempy <- tempy[!(gender == 'F' & age %in% 20:25) ]
  tempy <- tempy[!(height == 'Short' & age %in% 30:35) ]
  tempy <- tempy[!(weight == 'Heavy' & age %in% 40:45) ]

  expect_warning(age_standardize(ph.data = tempy,
                                 ref.popname = "2000 U.S. Std Population (11 age groups)",
                                 collapse = T,
                                 my.count = 'disease',
                                 my.pop = 'pop',
                                 per = 100000,
                                 conf.level = 0.95,
                                 by = c('height', 'weight', 'gender')),
                 'Missing ages detected in tempy for 7 groups.')

  # Warnings when only two groups
  temp.dt2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
                         count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
                         stdpop = rep(1000, 22))

  expect_warning(age_standardize(ph.data = temp.dt2,
                                 ref.popname = "none",
                                 collapse = F,
                                 my.count = "count",
                                 my.pop = "pop",
                                 per = 1000,
                                 conf.level = 0.95,
                                 by = "sex"),
                 'Missing ages detected in temp.dt2:')

  expect_warning(age_standardize(ph.data = temp.dt2,
                                 ref.popname = "none",
                                 collapse = F,
                                 my.count = "count",
                                 my.pop = "pop",
                                 per = 1000,
                                 conf.level = 0.95,
                                 by = "sex"),
                 'M 0-49, 61-100')


  # test if fails when have mismatched aggregated data
  set.seed(98104)
  tempy <- data.table(
    gender = sample(c('F', 'M'), 20000, replace = T),
    age = sample(0:100, 20000, replace = T),
    disease = sample(0:1, 20000, replace = T))
  tempy <- tempy[, .(pop = .N, disease = sum(disease)), .(gender, age)]
  tempy[age == 0, agecat := "0"] # notice these year bins are DIFFERENT from 2000 U.S. Std Population (11 age groups)
  tempy[age %in% 1:5, agecat := "1-5 years"]
  tempy[age %in% 6:14, agecat := "6-14 years"]
  tempy[age %in% 15:24, agecat := "15-24 years"]
  tempy[age %in% 25:34, agecat := "25-34 years"]
  tempy[age %in% 35:44, agecat := "35-44 years"]
  tempy[age %in% 45:54, agecat := "45-54 years"]
  tempy[age %in% 55:64, agecat := "55-64 years"]
  tempy[age %in% 65:74, agecat := "65-74 years"]
  tempy[age %in% 75:84, agecat := "75-84 years"]
  tempy[age %in% 85:100, agecat := "85+ years"]
  tempy <- tempy[, .(count = sum(disease), pop = sum(pop)), .(agecat, gender)]

  expect_error(age_standardize(ph.data = copy(tempy),
                               ref.popname = "2000 U.S. Std Population (11 age groups)",
                               collapse = F, # because already collapsed
                               my.count = "count",
                               my.pop = "pop",
                               per = 1000,
                               conf.level = 0.95,
                               by = "gender"),
               "The agecat values in ph.data must match those in your reference")

})

test_that("non-numeric age column rejected", {
  dt <- data.table(age = as.character(0:10), count = 1, pop = 10)
  expect_error(
    age_standardize(dt, my.count = "count", my.pop = "pop"),
    "The 'age' column must be numeric"
  )
})

test_that("error when ref.popname != 'none' but collapse = FALSE and no agecat", {
  dt <- data.table(age = 0:100, count = 1, pop = 100)
  expect_error(
    age_standardize(dt,
                    ref.popname = "2000 U.S. Std Population (11 age groups)",
                    collapse = FALSE, my.count = "count", my.pop = "pop"
    ),
    "When collapse = FALSE and ref.popname != 'none'"
  )
})

test_that("zero count data gives finite upper CI and lower CI == 0", {
  dt <- data.table(age = 0:100, count = 0, pop = 1000)
  res <- age_standardize(dt, my.count = "count", my.pop = "pop")
  expect_equal(res$adj.lci, 0)
  expect_true(is.finite(res$adj.uci))
})

test_that("line_level must be TRUE/FALSE, and requires collapse = TRUE", {
  dt <- data.table(age = 0:100, count = 1, pop = 100)
  expect_error(
    age_standardize(dt, my.count = "count", my.pop = "pop", line_level = "yes"),
    "`line_level` argument must be TRUE or FALSE"
  )
  dt_stdpop <- data.table(age = 0:100, count = 1, pop = 100, stdpop = 50)
  expect_error(
    age_standardize(dt_stdpop, my.count = "count", my.pop = "pop", collapse = FALSE,
                    line_level = TRUE, ref.popname = "none"),
    "`line_level = TRUE` requires `collapse = TRUE`"
  )
})

test_that("default (line_level = FALSE) warns but doesn't change how duplicate age rows are aggregated", {
  # This is like example #1 in the age_standardize.qmd vignette where the population denominator is inflated
  set.seed(98104)
  temp1 <- data.table(
    age = rep(51:60, 100),
    disease = sample(0:1, 1000, replace = TRUE),
    pop = rep(seq(1000, 910, -10), 100))

  expect_warning(
    age_standardize(ph.data = copy(temp1),
                    ref.popname = "2000 U.S. Std Population (11 age groups)",
                    collapse = TRUE, my.count = "disease", my.pop = "pop",
                    per = 1000, conf.level = 0.95),
    "multiple rows per 'age' combination"
  )

  default_result <- suppressWarnings(age_standardize(ph.data = copy(temp1),
                                                      ref.popname = "2000 U.S. Std Population (11 age groups)",
                                                      collapse = TRUE, my.count = "disease", my.pop = "pop",
                                                      per = 1000, conf.level = 0.95))
  expect_true(sum(default_result$pop) > 500000) # expect massive inflation
})

test_that("line_level = TRUE correctly aggregates line-level data so it matches manual pre-aggregation", {
  # Same temp1 as above, but opting in via line_level = TRUE.
  set.seed(98104)
  temp1 <- data.table(
    age = rep(51:60, 100),
    disease = sample(0:1, 1000, replace = TRUE),
    pop = rep(seq(1000, 910, -10), 100))

  expect_message(
    line_level_result <- suppressWarnings(age_standardize(ph.data = copy(temp1),
                                                           ref.popname = "2000 U.S. Std Population (11 age groups)",
                                                           collapse = TRUE, my.count = "disease", my.pop = "pop",
                                                           per = 1000, conf.level = 0.95, line_level = TRUE)),
    "ph.data \\(copy\\(temp1\\)\\) has 10 'age' combination\\(s\\) with multiple rows sharing"
  )

  expect_true(sum(line_level_result$pop) < 20000) # double check that we didn't inflate teh population denominator

  pre_aggregated <- temp1[, list(disease = sum(disease)), by = c("age", "pop")]
  manual <- suppressWarnings(age_standardize(ph.data = pre_aggregated,
                                             ref.popname = "2000 U.S. Std Population (11 age groups)",
                                             collapse = TRUE, my.count = "disease", my.pop = "pop",
                                             per = 1000, conf.level = 0.95))

  expect_equal(line_level_result$count, manual$count)
  expect_equal(line_level_result$pop, manual$pop)
  expect_equal(line_level_result$adj.rate, manual$adj.rate)
})

test_that("line_level = TRUE auto-aggregates duplicate age * by rows when pop is consistent within each group", {
  dt <- data.table(
    grp = rep(c("A", "B"), each = 4),
    age = rep(c(10, 20), each = 2, times = 2),
    count = c(1, 1, 2, 2, 3, 3, 4, 4),
    pop = c(100, 100, 200, 200, 300, 300, 400, 400))

  expect_message(
    result <- suppressWarnings(age_standardize(ph.data = copy(dt),
                                                ref.popname = "2000 U.S. Std Population (11 age groups)",
                                                collapse = TRUE, my.count = "count", my.pop = "pop",
                                                per = 1000, conf.level = 0.95, by = "grp", line_level = TRUE)),
    "ph.data \\(copy\\(dt\\)\\) has 4 'age' & 'grp' combination\\(s\\) with multiple rows sharing"
  )

  expect_equal(sum(result$count), sum(dt$count)) # double check the counts
  expect_equal(sum(result$pop), 1000) # be sure no double counting of pop. 100 + 200 + 300 + 400
})

test_that("sums pop when it varies across duplicate age rows, regardless of line_level", {
  dt <- data.table(
    gender = rep(c("F", "M"), each = 2),
    age = rep(c(10, 20), 2),
    count = c(1, 2, 3, 4),
    pop = c(100, 200, 150, 250))

  expect_warning(
    default_result <- age_standardize(ph.data = copy(dt),
                                      ref.popname = "2000 U.S. Std Population (11 age groups)",
                                      collapse = TRUE, my.count = "count", my.pop = "pop",
                                      per = 1000, conf.level = 0.95),
    "multiple rows per 'age' combination"
  )
  expect_equal(sum(default_result$count), sum(dt$count))
  expect_equal(sum(default_result$pop), sum(dt$pop))

  expect_no_message(
    line_level_result <- suppressWarnings(age_standardize(ph.data = copy(dt),
                                                          ref.popname = "2000 U.S. Std Population (11 age groups)",
                                                          collapse = TRUE, my.count = "count", my.pop = "pop",
                                                          per = 1000, conf.level = 0.95,
                                                          line_level = TRUE))
  )
  expect_equal(sum(line_level_result$count), sum(dt$count))
  expect_equal(sum(line_level_result$pop), sum(dt$pop))
})

test_that("age > 100 top-coding still correctly sums count & pop", {
  dt <- data.table(age = c(100, 103),
                   count = c(5, 7),
                   pop = c(1000, 2000))
  result <- suppressWarnings(age_standardize(ph.data = copy(dt),
                                             collapse = TRUE,
                                             my.count = "count",
                                             my.pop = "pop"))
  expect_equal(result$count, 12)
  expect_equal(result$pop, 3000) # should be summed b/c 100 & 103 are distinct ages before top-coding
})

test_that("no warning or message when ph.data is already one row per age (+ by)", {
  dt <- data.table(age = 0:100,
                   count = 1,
                   pop = 1000)

  expect_no_warning(
    age_standardize(dt,
                    ref.popname = "2000 U.S. Std Population (11 age groups)",
                    collapse = TRUE,
                    my.count = "count",
                    my.pop = "pop")
  )
  expect_no_message(
    age_standardize(dt,
                    ref.popname = "2000 U.S. Std Population (11 age groups)",
                    collapse = TRUE,
                    my.count = "count",
                    my.pop = "pop",
                    line_level = TRUE)
  )
})

