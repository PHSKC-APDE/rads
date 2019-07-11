library('testthat')
library('data.table')

#Simple recode of character -> character
test_that('Simple recode of a character -> character',{
  a = create_recode('a','b','a','b')
  d = data.table::data.table(a = 'a')
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 'b'))
})

#Test return as a vector
test_that('Simple recode of a character -> character; return as vector',{
  a = create_recode('a','b','a','b')
  d = data.table::data.table(a = 'a')
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(r,'b')
})

#Simple recode of numeric -> numeric
test_that('Simple recode of a numeric -> numeric',{
  a = create_recode('a','b',1,2)
  d = data.table::data.table(a = 1)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 2))
})

#Simple recode of logical -> logical
test_that('Simple recode of a Logical -> Logical',{
  a = hysdataprep::create_recode('a','b',TRUE, FALSE, simplify2numeric = F)
  d = data.table::data.table(a = TRUE)
  r = hysdataprep::apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = FALSE))
})

#Simple recode as a rename
test_that('Simple recode of as a rename',{
  a = hysdataprep::create_recode('a','b')
  d = data.table::data.table(a = 'canada')
  r = hysdataprep::apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 'canada'))
})

#NAs rather than null in old_value, new_value, and new_label
test_that('Simple recode of as a rename-- NA to NA',{
  a = hysdataprep::create_recode('a','b', NA, NA, NA)
  d = data.table::data.table(a = 1:10)
  r = hysdataprep::apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 1:10))
})

#Multiple character recodes at once
test_that('Recoding multiple values at once, all non-NA',{
  a = create_recode('a','b',1:3, c('3','4','5'), simplify2numeric = F)
  d = data.table::data.table(a = 1:3)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = c('3','4','5')))
})

#multiple recodes at once, with offset NAs
test_that('Recoding multiple values at once, offset NAs',{
  a = create_recode('a','b',c(1:2, NA), c(NA,'4','5'), simplify2numeric = F)
  d = data.table::data.table(a = 1:3)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = c(NA,'4',NA)))
})

#numeric -> with labels
test_that('Recoding numeric to have labels',{
  a = create_recode('a','b', old_value = 1:5, new_value = 1:5, new_label = letters[1:5])
  d = data.table(a = 1:5)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(factor(1:5, 1:5, letters[1:5]), r)
})

#recoding over an existing factor, complete replacement
test_that('Recoding over factor variable-- specifing numeric representation',{
  a = create_recode('a','a', old_value = 1:3, new_value = 1:3, new_label = c('d','e','f'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(1:3, 1:3, letters[4:6]), r)
})

test_that('Recoding over factor variable-- specifing numeric representation and changing the underlying numeric',{
  a = create_recode('a','a', old_value = 1:3, new_value = 4:6, new_label = c('d','e','f'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(4:6, 4:6, letters[4:6]), r)
  expect_equal(as.numeric(r), 4:6)
})

test_that('Recoding over factor variable-- reassign a label but lose 1:1 value:label mapping',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','c'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  expect_error(apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T),
               "The following pairs of values:labels indicate duplicate mappings. This is likely caused by a partial overwrite of a variable (e.g. old_var == new_var). 3:c, 10:c",
               fixed = T)

})

test_that('Recoding over factor variable-- partial update',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','b'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(c(9, 10, 3), c(3,9,10), c('c', 'a','b')), r)
})

#do some tests with lfactors
test_that('Recoding over factor variable-- specifing numeric representation',{
  a = create_recode('a','a', old_value = 1:3, new_value = 1:3, new_label = c('d','e','f'))
  d = data.table(a = lfactors::lfactor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(1:3, 1:3, letters[4:6]), r)
})

test_that('Recoding over factor variable-- specifing numeric representation and changing the underlying numeric',{
  a = create_recode('a','a', old_value = 1:3, new_value = 4:6, new_label = c('d','e','f'))
  d = data.table(a = lfactors::lfactor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(4:6, 4:6, letters[4:6]), r)
  expect_equal(as.numeric(r), 4:6)
})

test_that('Recoding over factor variable-- reassign a label but lose 1:1 value:label mapping',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','c'))
  d = data.table(a = lfactors::lfactor(1:3, 1:3, c('a','b','c')))
  expect_error(apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T),
               "The following pairs of values:labels indicate duplicate mappings. This is likely caused by a partial overwrite of a variable (e.g. old_var == new_var). 3:c, 10:c",
               fixed = T)

})

test_that('Recoding over factor variable-- partial update',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','b'))
  d = data.table(a = lfactors::lfactor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F, return_vector = T)
  expect_equal(lfactors::lfactor(c(9, 10, 3), c(3,9,10), c('c', 'a','b')), r)
})




# test_that('Recoding multiple values at once, all non-NA, adding labels with padding',{
#   a = create_recode('a','b', old_value = 1:3, new_value = c('3','4','5'), new_label = c('a','b','c'))
#   d = data.table::data.table(a = 1:3)
#   r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
#   expect_equal(as.numeric(r[, b]), 4:6)
#   expect_equal(as.character(r[,b]), c('a','b','c'))
# })

#Multiple 1 : 1 numeric recodes at once

#Numeric recoding using binning

#Test for side effect of adding a column

#Simple recode with an overwrite

#Simple recode with an overwrite that jumps from numeric -> character

#Simple recode overwritting factor labels

#Simple recode adding new values and labels

#Binned recoding of factors

#Expected error of passing instructions

#figure out what to do with factors and how we can preserve the underlying numeric representation of the variable
#e.g. is a factor of 2, 3, 4 represented as 1:3?

#simple recode tests
a = create_recode('V1', 'V2', 'a', 'b', 'b', '', '')
b = data.table('a')
t1 = apply_recode(b, 2016, a, F)
t2 = apply_recode(b, 2016, a, T)

#more complex
data_vector = factor(rep(letters[1:3],3),letters[1:3])
old_vals = letters[1:2]
new_vals = LETTERS[1:2]
d = data.table(V1 = data_vector)

#creating a new column
rec = create_recode('V1', 'V2', old_vals, new_vals, new_label = new_vals)
e = apply_recode(d, 2016, rec)

#overwriting and existing one
d1 = copy(d)
d1[, V2:=V1]
rec = create_recode('V1', 'V2', old_vals, new_vals, new_label = new_vals)
e1 = apply_recode(d1, 2016, rec)

#recoding with binning
d2 = data.table(v1 = c(NA, 1:10))
rec2 = create_recode('v1', 'v2', old_value = c('[1,2]','(3,4]', '(4,6)', '[6,7)', 8, 9, NA),
                                 new_value = c(-1,-2,-3,-4,-5,-6,-7))
e2 = apply_recode(d2, 2016, rec2)

#what happens when try to recode a factor(e.g. the label) rather than the value?
