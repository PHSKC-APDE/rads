library(testthat)
library(data.table)

test_that("create_dictionary handles invalid inputs correctly", {
  expect_error(create_dictionary(list(x = 1:3), source = "test"), "Input must be a data.frame or data.table")
  expect_error(create_dictionary(data.table(x = 1:3), source = ""), "source must not be an empty string")
  expect_error(create_dictionary(data.table(x = 1:3), source = c("test1", "test2")), "source must be a character vector of length one")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", suppress = "y"), "The following are not valid column names")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", suppress = c("x", "x")), "suppress must not contain duplicate column names")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", varsort = "yes"), "varsort must be a logical vector of length one")  
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", max_unique_values = -1), "max_unique_values must be a positive integer")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", max_unique_values = 1.5), "max_unique_values must be a positive integer")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", truncation_threshold = 0), "truncation_threshold must be a positive integer")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", max_unique_values = 5, truncation_threshold = 6), "max_unique_values must be greater than or equal to truncation_threshold")
  
  # Confirm use of valid ph.ref 
  invalid_ph_desc <- data.table(source = "test", 
                                varname = "x", 
                                desc = 'just a description', 
                                notes = 'just a note')
  expect_equal(unique(create_dictionary(data.table(x = 1:3), source = "test", ph.ref = invalid_ph_desc)[]$desc), c('just a description'))
  expect_equal(unique(create_dictionary(data.table(x = 1:3), source = "test", ph.ref = invalid_ph_desc)[]$notes), c('just a note'))
  
  # Test for invalid ph.ref input - insufficient columns
  invalid_ph_desc <- data.table(source = "test", 
                                varname = "x")
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", ph.ref = invalid_ph_desc), "ph.ref must contain the following columns: source, varname, desc")
  
  # Test for invalid ph.ref input - non-matching source name
  invalid_ph_desc <- data.table(source = "test_test", 
                                varname = "x", 
                                desc = 'just a test')
  expect_error(create_dictionary(data.table(x = 1:3), source = "test", ph.ref = invalid_ph_desc), "Error in `source` argument or `ph.ref` data")
  
})

test_that("create_dictionary handles basic input correctly", {
  # Create a sample dataset
  dt <- data.table(
    x = 1:5,
    y = letters[1:5],
    z = factor(c("low", "medium", "high", "low", "medium"))
  )
  
  result <- create_dictionary(dt, source = "test")
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 13)
  expect_identical(unique(result$varname), c("x", "y", "z"))
  expect_identical(unique(result$vartype), c("integer", "character", "factor"))
  expect_identical(sort(result$factor_labels), c('high', 'low', 'medium'))
})

test_that("create_dictionary handles different variable types correctly", {
  dt <- data.table(
    logical = c(TRUE, FALSE),
    date = as.Date(c("2023-01-01", "2023-01-02")),
    datetime = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 13:00:00")),
    other = list(1:3, 4:6)
  )
  
  result <- create_dictionary(dt, source = "test")
  
  expect_equal(unique(result$vartype), c("logical", "date", "datetime", "other"))
  expect_true(grepl("check dataset:", result[varname == "other"]$values))
})

test_that("create_dictionary handles suppression correctly", {
  dt <- data.table(
    sensitive = c("A123", "B456", "C789"),
    normal = 1:3
  )
  
  result <- create_dictionary(dt, source = "test", suppress = "sensitive")
  
  expect_equal(result[varname == "sensitive"]$values, "_suppressed_")
  expect_equal(result[varname == "normal"]$values, c("1", "2", "3"))
})

test_that("create_dictionary handles varsort correctly", {
  dt <- data.table(c = 1:3, a = 4:6, b = 7:9)
  
  result_unsorted <- create_dictionary(dt, source = "test", varsort = FALSE)
  result_sorted <- create_dictionary(dt, source = "test", varsort = TRUE)
  
  expect_equal(unique(result_unsorted$varname), c("c", "a", "b"))
  expect_equal(unique(result_sorted$varname), c("a", "b", "c"))
})

test_that("create_dictionary handles max_unique_values correctly", {
  dt <- suppressWarnings(data.table(
    few = c(1, 2, 3, 4),
    many = 1:10
  ))
  
  result <- create_dictionary(dt, source = "test", max_unique_values = 5)
  
  expect_equal(length(result[varname == "few"]$values), 4)
  expect_true(grepl("min =.*max =", result[varname == "many"]$values))
})

test_that("create_dictionary handles truncation_threshold correctly", {
  dt <- data.table(
    many_values = letters[1:10]
  )
  
  # Test with default truncation_threshold (5)
  result_default <- create_dictionary(dt, source = "test", max_unique_values = 8)
  expect_equal(length(unique(result_default$values)), 6)  # 5 values + "..."
  
  # Test with custom truncation_threshold
  result_custom <- create_dictionary(dt, source = "test", max_unique_values = 8, truncation_threshold = 3)
  expect_equal(length(unique(result_custom$values)), 4)  # 3 values + "..."
  
  # Test when number of unique values is less than max_unique_values but more than truncation_threshold
  dt_few <- data.table(few_values = letters[1:7])
  result_few <- create_dictionary(dt_few, source = "test", max_unique_values = 8, truncation_threshold = 5)
  expect_equal(length(unique(result_few$values)), 7)  # All 7 values should be shown
  
  # Test when truncation_threshold equals max_unique_values
  result_equal <- create_dictionary(dt, source = "test", max_unique_values = 5, truncation_threshold = 5)
  expect_equal(length(unique(result_equal$values)), 6)  # 5 values + "..."
})

test_that("create_dictionary handles ph.ref correctly", {
  dt <- data.table(x = 1:3, y = letters[1:3])
  ph.ref <- data.table(
    source = rep("test", 2),
    varname = c("x", "y"),
    desc = c("X variable", "Y variable"),
    notes = c("Important", "Check values")
  )
  
  result <- create_dictionary(dt, source = "test", ph.ref = ph.ref)
  
  expect_identical(unique(result$desc), c("X variable", "Y variable"))
  expect_identical(unique(result$notes), c("Important", "Check values"))
})

test_that("create_dictionary handles factor variables correctly", {
  dt <- data.table(
    f = factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
  )
  
  result <- create_dictionary(dt, source = "test")
  
  expect_equal(unique(result$vartype), "factor")
  expect_equal(result$values, c("1", "2", "3"))
  expect_equal(result$factor_labels, c("low", "medium", "high"))
})

test_that("create_dictionary handles binary variables correctly", {
  dt <- data.table(
    binary = c(0, 1, 0, 1)
  )
  
  result <- create_dictionary(dt, source = "test")
  
  expect_equal(unique(result$vartype), "binary")
  expect_equal(unique(result$values), c("0", "1"))
})