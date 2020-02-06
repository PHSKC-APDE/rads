library('testthat')
library('data.table')
library('apdeRecodes')
#Simple recode of character -> character
test_that('Simple recode of a character -> character',{
  r = do_recode('a', 'a', 'b')
  expect_equal(r,'b')
})


#Simple recode of numeric -> numeric
test_that('Simple recode of a numeric -> numeric',{
  r = do_recode(1,1,2)
  expect_equal(r,2)
})

#Simple recode of logical -> logical
test_that('Simple recode of a Logical -> Logical',{
  r = do_recode(1,1,2)
  expect_equal(r,2)
})


#Multiple character recodes at once
test_that('Recoding multiple values at once, all non-NA',{
  r = do_recode(1:3, 1:3, as.character(3:5))
  expect_equal(r, c('3','4','5'))
})

#multiple recodes at once, with offset NAs
test_that('Recoding multiple values at once, offset NAs',{
  r = do_recode(1:3, c(1:2, NA), c(NA, '4','5'))
  expect_equal(r, c(NA, '4', NA))
})

#numeric -> with labels
test_that('Recoding numeric to have labels',{
  r = do_recode(1:5, 1:5, 1:5, letters[1:5])
  expect_equal(factor(1:5, 1:5, c('a','b','c','d','e')), r)
})

#recoding over an existing factor, complete replacement
test_that('Recoding over factor variable-- specifing numeric representation',{
  r = do_recode(factor(1:3, 1:3, c('a','b','c')), 1:3, c('d','e','f'), c('d','e','f'))
  expect_equal(factor(1:3, 1:3, c('d','e','f')), r)
})

#This test is less useful for when factors are the main output
test_that('Recoding over factor variable-- specifing numeric representation and changing the underlying numeric',{
  r = do_recode(factor(1:3, 1:3, c('a','b','c')), 1:3, 4:6, c('d','e','f'))
  expect_equal(factor(4:6, 4:6, c('d','e','f')), r)
  expect_equal(as.numeric(r), 1:3)
})


test_that('Recoding over factor variable-- partial update',{
  r = do_recode(factor(1:3, 1:3, c('a','b','c')), 1:2, 9:10, c('d','e'), update = T)
  expect_equal(factor(c(9,10, 3), c(9,10, 3), c('d', 'e','c')), r)
})


test_that('Factor in factor out with shifting values',{
  r = do_recode(factor(1:3,1:3, c('a','b','c')), 1:3, 4:6, c('d','e','f'))
  expect_equal(factor(1:3,1:3, c('d','e','f')), r)
})

test_that('binned recodes',{
  r = do_recode(1:10, c('(0,1]', '[2 , 5]', '[6 ,8)', 8, '(8, 10]'), c(1,2,3,4,5))
  expect_equal(c(1,2,2,2,2,3,3,4,5,5), r)
})

test_that('When new_label is all NA, no labels get made',{
  r = do_recode(1:3, 1:3, 4:6, c(NA, NA, NA), update = TRUE)
  expect_equal(4:6, r)
})

test_that('improperly specified binned recodes',{
  r = try(do_recode(1,'[0-1]', 2), silent = TRUE)
  expect_equal(TRUE, grepl('Error in do_recode', r))
})


# test_that('Recode starting data without labels and then recode on top of that',{
#   d = data.table(a = 1:3)
#   r1 = do_recode(1:3, 1, NA_real_, NA)
#   r2 =
#   r2 = create_recode('a', 'b', old_value = c(2:3), new_value = 0:1, new_label = c('No','Yes'))
#   apply_recode(data = d, recode = r1, jump_scope = T, return_vector = T)
#
#   res = apply_recode(data = d,  recode = r2, jump_scope = F, return_vector = T)
#   expect_equal(res, factor(c(NA, 0, 1), c(0,1), c('No', "Yes")))
# })
#
# test_that('NUll new labels, but old labels exist', {
#   d = data.table(kc4reg = factor(c('East', 'Seattle', 'North', 'South')))
#   r = create_recode('kc4reg', '.Region', old_value = as.character(d[, kc4reg]), new_value = c('NS', 'S', 'NS', 'NS'))
#   res = apply_recode(d, r, F, T)
#   expect_equal(res, c('NS', 'S', 'NS', 'NS'))
# })

