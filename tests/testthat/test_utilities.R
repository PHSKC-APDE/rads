library('testthat')
library(DBI)

# adjust_direct() ----
test_that('adjust_direct',{

  temp.direct <- adjust_direct(count = c(11, 9), pop = c(500, 500), stdpop = c(640, 720), per = 100, conf.level = 0.95)

  expect_equal(20, temp.direct[["count"]] )

  expect_equal(2, temp.direct[["crude.rate"]] )

  expect_equal(1.2217, round(temp.direct[["crude.lci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(3.0888, round(temp.direct[["crude.uci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(1.9882, round(temp.direct[["adj.rate"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(1.2133, round(temp.direct[["adj.lci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(3.0820, round(temp.direct[["adj.uci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

})

# age_standardize() ----
test_that('age_standardize',{
  temp.dt1 <- data.table(age = c(50:60), count = c(25:35), pop = c(seq(1000, 800, -20)) )
  temp.agestd1 <- suppressWarnings(age_standardize(ph.data = temp.dt1, ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T,
                  my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95))

  expect_equal(sum(temp.dt1$count), temp.agestd1[["count"]])

  expect_equal(round(1000*sum(temp.dt1$count) / sum(temp.dt1$pop), 2), temp.agestd1[["crude.rate"]])

  expect_equal(29.83, temp.agestd1[["crude.lci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(37.13, temp.agestd1[["crude.uci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.10, temp.agestd1[["adj.rate"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(30.62, temp.agestd1[["adj.lci"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(40.25, temp.agestd1[["adj.uci"]] ) # checked vis-à-vis epitools::ageadjust.direct


  temp.dt2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
                      count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
                      stdpop = rep(1000, 22))
  temp.agestd2 <- suppressWarnings(age_standardize(ph.data = temp.dt2, ref.popname = "none", collapse = F, my.count = "count",
                  my.pop = "pop", per = 1000, conf.level = 0.95, group_by = "sex"))

  expect_equal(sum(temp.dt2[sex == "M"]$count) , temp.agestd2[sex == "M"]$count)
  expect_equal(sum(temp.dt2[sex == "F"]$count) , temp.agestd2[sex == "F"]$count)

  expect_equal(round(1000*sum(temp.dt2[sex == "M"]$count) / sum(temp.dt2[sex == "M"]$pop), 2) , temp.agestd2[sex == "M"]$crude.rate)
  expect_equal(round(1000*sum(temp.dt2[sex == "F"]$count) / sum(temp.dt2[sex == "F"]$pop), 2) , temp.agestd2[sex == "F"]$crude.rate)

  expect_equal(28.26 , temp.agestd2[sex == "M"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(26.47 , temp.agestd2[sex == "F"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.18 , temp.agestd2[sex == "M"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(32.83 , temp.agestd2[sex == "F"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(31.73 , temp.agestd2[sex == "M"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(29.64 , temp.agestd2[sex == "F"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(28.39 , temp.agestd2[sex == "M"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(26.58 , temp.agestd2[sex == "F"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(35.35 , temp.agestd2[sex == "M"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(32.97 , temp.agestd2[sex == "F"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct

  # test for errors and warnings
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

  })

# convert_to_date() ----
# Test that common date formats are parsed correctly
test_that("common date formats are parsed correctly", {
  expect_equal(convert_to_date("2024-01-01"), as.Date("2024-01-01"))
  expect_equal(convert_to_date("2024/02/01"), as.Date("2024-02-01"))
  expect_equal(convert_to_date("03-01-2024"), as.Date("2024-03-01"))
  expect_equal(convert_to_date("04/01/2024"), as.Date("2024-04-01"))
  expect_equal(convert_to_date("2024-03-05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("2024/03/05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("March 10, 2024"), as.Date("2024-03-10"))
  expect_equal(convert_to_date("10 March 2024"), as.Date("2024-03-10"))
})

# Test that numeric values are converted correctly using different origins
test_that("numeric values are converted correctly using default and custom origins", {
  expect_equal(convert_to_date(0), as.Date("1899-12-30"))
  expect_equal(convert_to_date(1), as.Date("1899-12-31"))
  expect_equal(convert_to_date(43500, origin = "1900-1-1"), as.Date("2019-02-06"))
})

# Test handling of non-date strings
test_that("non-date strings return NA and a warning", {
  expect_warning(out <- convert_to_date(c("dogs", "cats")),
                 "cannot be converted to a date")
  expect_equal(out, c("dogs", "cats"))
})

# Test that origin must be in %Y-%m-%d format
test_that("origin must be in %Y-%m-%d format", {
  expect_error(convert_to_date(43500, origin = "01-01-1900"),
               "Origin date must be in '%Y-%m-%d' format.")
  expect_error(convert_to_date(43500, origin = "1900/1/1"),
               "Origin date must be in '%Y-%m-%d' format.")
})

# Test with real data and mixed valid/invalid dates
test_that("real data with mixed valid and invalid dates handles correctly", {
  mixed_dates <- c("2024-01-01", "invalid date", "2024-12-31", "not a date")
  expected_dates <- as.Date(c("2024-01-01", NA, "2024-12-31", NA))
  result <- convert_to_date(mixed_dates)
  expect_equal(result, expected_dates)
})


# format_time() ----
test_that('format_time',{

  expect_equal('2010', format_time(2010))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_time(c(2000, 2014:2016, 3000, 3002:4000)))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_time(c(3002:4000, 2000, 2014:2016, 3000)))


})

# get_ref_pop() ----
test_that('get_ref_pop',{

  temp.pop <- get_ref_pop("2000 U.S. Std Population (19 age groups - Census P25-1130)")

  expect_equal(19, nrow(temp.pop))

  expect_equal(5, ncol(temp.pop))

  expect_equal(c("age_end", "age_start", "agecat", "pop", "ref_pop_name"), sort(names(temp.pop)))

})

# list_ref_pop() ----
test_that('list_ref_pop',{

  expect_equal(36, length(list_ref_pop()))

})

# lossless_convert() ----
test_that('lossless_convert', {
  expect_equal(class(lossless_convert(c('1', '2', '3'), 'integer')), 'integer')

  expect_equal(
    expect_message(
      lossless_convert(c('one', '2', '3'), 'integer'),
      'would introduce additional NAs'),
    c('one', '2', '3'))

  expect_equal(
    expect_message(
      lossless_convert(c('1', '2', 'three'), 'integer'),
      'would introduce additional NAs'),
    c('1', '2', 'three'))

  expect_equal(class(lossless_convert(c('2020-01-01', '2021-12-31', '2022-02-22'), 'Date')), 'Date')

  expect_equal(
    expect_message(
      lossless_convert(c('2020-01-01', '2021-12-31', 'z'), 'Date'),
    'would introduce additional NAs'),
  c('2020-01-01', '2021-12-31', 'z'))

  expect_equal(
    expect_message(
      lossless_convert(c('z', '2020-01-01', '2021-12-31'), 'Date'),
    'would introduce additional NAs'),
  c('z', '2020-01-01', '2021-12-31'))

})

# std_error() ----
test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

# tsql_chunk_loader() ----
# set up tsql_chunk_loader test data
mydt = data.table(col1 = 1:10000L,  # create integer
                  col2 = 1:10000/3) # create float
mydt[, col3 := as.Date(Sys.Date()) - col1] # create date
mydt[, col4 := as.character(col3)] # create string
myconn = validate_hhsaw_key()

test_that("tsql_chunk_loader works correctly and congratulates on success", {
  expect_message(
    tsql_chunk_loader(
      ph.data = mydt,
      db_conn = myconn,
      chunk_size = 3333,
      schema_name = Sys.getenv("USERNAME"),
      table_name = 'justTesting',
      overwrite = TRUE,
      append = FALSE,
      field_types = c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
      validate_field_types = TRUE,
      validate_upload = TRUE
    ),
    regexp = "Congratulations"
  )
  expect_message(
    tsql_chunk_loader(
      ph.data = mydt,
      db_conn = myconn,
      chunk_size = 3333,
      schema_name = Sys.getenv("USERNAME"),
      table_name = 'justTesting',
      overwrite = FALSE,
      append = TRUE,
      field_types = c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
      validate_field_types = TRUE,
      validate_upload = TRUE
    ),
    regexp = "Congratulations"
  )
})

test_that("error if ph.data is NULL", {
  expect_error(tsql_chunk_loader(ph.data = NULL), "must specify a dataset")
})

test_that("error if ph.data is not a data.frame or data.table", {
  expect_error(tsql_chunk_loader(ph.data = list()), "must be the name of a data.frame or data.table.")
})

test_that("error if db_conn is NULL", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = NULL), "must be specified")
})

test_that("error if db_conn is not a MS SQL Server object", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = mydt), 'not a "Microsoft SQL Server" object')
})

test_that("error if chunk_size is not an integer between 100 and 20,000", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, chunk_size = 50), "must be an integer between 100 and 20,000")
})

test_that("error if schema_name is not a single quoted name", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema_name = c("invalid", "name")), "must be a quoted name of a single schema")
})

test_that("error if table_name is not a single quoted name", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("invalid", "name")), "must be a quoted name of a single table")
})

test_that("error if overwrite and append are both TRUE or FALSE", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = TRUE), "cannot both be set to the same value")
})

test_that("error if field_types do not match column names in ph.data", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                field_types = c(wrong = 'int')), "must match the column names")
})

test_that("error if field_types are not compatible with classes in ph.data", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                 table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                 field_types =  c(col1 = 'date', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
                                 validate_field_types = TRUE), "did not align with the proposed TSQL datatypes")
})

test_that("error if validate_upload is not logical", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, chunk_size = 180, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
                                validate_upload = NULL), "must be specified as a logical")
})

# tsql_validate_field_types ----
test_that("function succeeds with compatible data.table and field types", {
  mydt <- data.table(col1 = 1:10, col2 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Success")
})

test_that("function fails with incompatible field types", {
  mydt <- data.table(col1 = 1:10, col2 = as.Date('2023-01-01'))
  my_field_types <- c(col1 = 'int', col2 = 'nvarchar(12)')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types))
})

test_that("function handles NULL arguments appropriately", {
  mydt <- data.table(col1 = 1:10, col2 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = NULL))
  expect_error(tsql_validate_field_types(ph.data = NULL, field_types = my_field_types))
})

test_that("function converts data.frame to data.table and succeeds", {
  mydf <- data.frame(col1 = 1:10, col2 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_message(tsql_validate_field_types(ph.data = mydf, field_types = my_field_types), "Success")
})

test_that("function fails when field types and data.table column names do not match", {
  mydt <- data.table(col1 = 1:10, col3 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types))
})

test_that("function handles unsupported R data types gracefully", {
  mydt <- data.table(col1 = 1:10, col2 = list(1:10))
  my_field_types <- c(col1 = 'int', col2 = 'int')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types))

  mydt <- data.table(col1 = 1:10, col2 = as.character(1:10))
  my_field_types <- c(col1 = 'int', col2 = 'unsupported')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types))
})
