library('testthat')
library('data.table')

#Simple recode of character -> character
test_that('Simple recode of a character -> character',{
  a = create_recode('a','b','a','b')
  d = data.table::data.table(a = 'a')
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 'b'))
})

#Simple recode of numeric -> numeric
test_that('Simple recode of a numeric -> numeric',{
  a = create_recode('a','b',1,2)
  d = data.table::data.table(a = 1)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 2))
})

#Simple recode of numeric -> factor
test_that('Simple recode of a factor -> factor',{
  a = create_recode('a','b',1, 2, 'b')
  d = data.table::data.table(a = 1)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = factor(2, 2, 'b')))
})

#Simple recode of logical -> logical
test_that('Simple recode of a factor -> factor',{
  a = create_recode('a','b',TRUE, FALSE)
  d = data.table::data.table(a = TRUE)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = FALSE))
})

#Simple recode as a rename
test_that('Simple recode of a factor -> factor',{
  a = create_recode('a','b',TRUE, FALSE)
  d = data.table::data.table(a = TRUE)
  r = apply_recode(data = d, year = 2018, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = FALSE))
})

#Multiple character recodes at once

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
