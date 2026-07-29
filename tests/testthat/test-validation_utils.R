library('testthat')

test_that('depth() returns 0 for non-list vectors and empty lists', {
  expect_equal(depth(c(1, 2, 3)), 0)
  expect_equal(depth(list()), 0)
  expect_equal(depth('a'), 0)
})

test_that('depth() counts nested list layers correctly', {
  expect_equal(depth(list(a = 1:3, b = letters[1:5])), 1)
  expect_equal(depth(list(a = list(x = 1, y = 2), b = 3)), 2)

  nested_list <- list(level1 = list(level2 = list(level3 = c(1, 2, 3))))
  expect_equal(depth(nested_list), 3)
})

test_that('depth() returns the maximum depth across mixed branches', {
  mixed_list <- list(shallow = c(1, 2), deep = list(inner = list(deeper = 42)))
  expect_equal(depth(mixed_list), 3)
})

test_that('validate_list_input() wraps a vector into a list and generates names', {
  r <- validate_list_input(x = c('a', 'b'), values = c('a', 'b', 'c'), variable_name = 'myvar')
  expect_type(r, 'list')
  expect_length(r, 1)
  expect_false(is.null(names(r)))
})

test_that('validate_list_input() defaults to unique(values) when x is NULL', {
  r <- validate_list_input(x = NULL, values = c('a', 'b', 'a', NA), variable_name = 'myvar')
  expect_equal(sort(r[[1]]), c('a', 'b'))
})

test_that('validate_list_input() errors on non-mutually-exclusive groupings', {
  expect_error(validate_list_input(x = list(c('a', 'b'), c('b', 'c')),
                                    values = c('a', 'b', 'c'),
                                    variable_name = 'myvar'))
})

test_that('validate_list_input() errors when x contains values not in `values`', {
  expect_error(validate_list_input(x = c('a', 'z'), values = c('a', 'b', 'c'), variable_name = 'myvar'))
})
