library('testthat')
library('data.table')
library('labelled')

#Simple recode of character -> character
test_that('Simple recode of a character -> character',{
  a = create_recode('a','b','a','b')
  d = data.table::data.table(a = 'a')
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 'b'))
})

#Test return as a vector
test_that('Simple recode of a character -> character; return as vector',{
  a = create_recode('a','b','a','b')
  d = data.table::data.table(a = 'a')
  r = apply_recode(data = d,  recode = a, jump_scope = F, return_vector = T)
  expect_equal(r,'b')
})

#Simple recode of numeric -> numeric
test_that('Simple recode of a numeric -> numeric',{
  a = create_recode('a','b',1,2)
  d = data.table::data.table(a = 1)
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 2))
})

#Simple recode of logical -> logical
test_that('Simple recode of a Logical -> Logical',{
  a = create_recode('a','b',TRUE, FALSE, simplify2numeric = F)
  d = data.table::data.table(a = TRUE)
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = FALSE))
})

#Simple recode as a rename
test_that('Simple recode of as a rename',{
  a = create_recode('a','b')
  d = data.table::data.table(a = 'canada')
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 'canada'))
})

#NAs rather than null in old_value, new_value, and new_label
test_that('Simple recode of as a rename-- NA to NA',{
  a = create_recode('a','b', NA, NA, NA)
  d = data.table::data.table(a = 1:10)
  r = apply_recode(data = d,  recode = a, jump_scope = F)
  expect_equal(r,data.table(b = 1:10))
})

#Multiple character recodes at once
test_that('Recoding multiple values at once, all non-NA',{
  a = create_recode('a','b',1:3, c('3','4','5'), simplify2numeric = F)
  d = data.table::data.table(a = 1:3)
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = c('3','4','5')))
})

#multiple recodes at once, with offset NAs
test_that('Recoding multiple values at once, offset NAs',{
  a = create_recode('a','b',c(1:2, NA), c(NA,'4','5'), simplify2numeric = F)
  d = data.table::data.table(a = 1:3)
  r = apply_recode(data = d, recode = a, jump_scope = F)
  expect_equal(r,data.table(b = c(NA,'4',NA)))
})

#numeric -> with labels
test_that('Recoding numeric to have labels',{
  a = create_recode('a','b', old_value = 1:5, new_value = 1:5, new_label = letters[1:5])
  d = data.table(a = 1:5)
  r = apply_recode(data = d,recode = a, jump_scope = F, return_vector = T)
  expect_equal(labelled::labelled(1:5, c(a = 1, b = 2, c = 3, d = 4, e = 5)), r)
})

#recoding over an existing factor, complete replacement
test_that('Recoding over factor variable-- specifing numeric representation',{
  a = create_recode('a','a', old_value = 1:3, new_value = 1:3, new_label = c('d','e','f'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, recode = a, jump_scope = F, return_vector = T)
  expect_equal(labelled::labelled(as.numeric(1:3), c(d = 1, e = 2, f = 3)), r)
})

test_that('Recoding over factor variable-- specifing numeric representation and changing the underlying numeric',{
  a = create_recode('a','a', old_value = 1:3, new_value = 4:6, new_label = c('d','e','f'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d, recode = a, jump_scope = F, return_vector = T)
  expect_equal(labelled(4:6, c(d = 4, e = 5, f = 6)), r)
  expect_equal(as.numeric(r), 4:6)
})

test_that('Recoding over factor variable-- reassign a label but lose 1:1 value:label mapping',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','c'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  expect_error(apply_recode(data = d, recode = a, jump_scope = F, return_vector = T),
               "The following pairs of values:labels indicate duplicate mappings. This is likely caused by a partial overwrite of a variable (e.g. old_var == new_var). 3:c, 10:c",
               fixed = T)

})

test_that('Recoding over factor variable-- partial update',{
  a = create_recode('a','a', old_value = 1:2, new_value = c(9,10), new_label = c('a','b'))
  d = data.table(a = factor(1:3, 1:3, c('a','b','c')))
  r = apply_recode(data = d,recode = a, jump_scope = F, return_vector = T)
  expect_equal(labelled(c(9, 10, 3), c(c = 3, a = 9,b = 10)), r)
})


test_that('Labelled in Labelled out with shifting values',{
  a = create_recode('a', 'a', old_value = 1:3, new_value = 4:6, new_label = c('d','e','f'))
  d = data.table(a = labelled(1:3, c(a = 1, b = 2, c = 3)))
  r = apply_recode(data = d, recode = a, jump_scope = F, return_vector = T)
  expect_equal(labelled(4:6, c(d = 4, e = 5, f = 6)), r)
})

test_that('binned recodes',{
  a = create_recode('a', 'a', old_value = c('(0,1]', '[2 , 5]', '[6 ,8)', 8, '(8, 10]'), new_value = c(1,2,3,4,5))
  d = data.table(a = 1:10)
  r = apply_recode(data = d, recode = a, jump_scope = F, return_vector = T)
  expect_equal(c(1,2,2,2,2,3,3,4,5,5), r)
})

test_that('improperly specified binned recodes are warned recodes',{
  a = create_recode('a', 'a', old_value = c('[0-1]'), new_value = 2)
  d = data.table(a = 1)
  expect_warning(apply_recode(data = d, recode = a, jump_scope = F, return_vector = T),
               'Did you mean')
})

test_that('Recode starting data without labels and then recode on top of that',{
  d = data.table(a = 1:3)
  r1 = create_recode('a', 'a', old_value = 1, new_value = NA, new_label = NA)
  r2 = create_recode('a', 'b', old_value = c(2:3), new_value = 0:1, new_label = c('No','Yes'))
  apply_recode(data = d, recode = r1, jump_scope = T, return_vector = T)
  res = apply_recode(data = d,  recode = r2, jump_scope = F, return_vector = T)
  expect_equal(res, labelled::labelled(c(NA, 0, 1), setNames(c(0, 1), c('No', 'Yes'))))
})



