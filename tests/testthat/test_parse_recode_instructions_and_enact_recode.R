library('data.table')
library('testthat')
i1 = data.table(old_var = 'a1', new_var = 'b1', old_value = 1:3, new_value = 4:6, new_label = NA)
i2 = data.table(old_var = 'a2', new_var = 'b2', old_value = 1:3, new_value = 4:6, new_label = c('a','b','c'))
i3 = data.table(old_var = 'a3', new_var = 'a3', old_value = 1:3, new_value = 4:6, new_label = NA)
i4 = data.table(old_var = 'a4', new_var = 'b4', old_value = c('1','2','3'), new_value = c('4','5','6'), new_label = NA)
i5 = data.table(old_var = 'a5', new_var = 'b5', old_value = NA, new_value = NA, new_label = NA)
i = rbind(i1,i2,i3,i4, i5)

#the rep(NA,3) is not really needed to make the do_recode results the same
#but is needed to make the create recodes the same
r1 = create_recode('a1','b1', 1:3,4:6, rep(NA_character_,3))
r2 = create_recode('a2','b2', 1:3, 4:6, c('a','b','c'))
r3 = create_recode('a3','a3', 1:3, 4:6, rep(NA_character_,3))
r4 = create_recode('a4', 'b4', c('1','2','3'), c('4','5','6'), rep(NA_character_,3), simplify_to_numeric = TRUE)
r5 = create_recode('a5', 'b5', NA, NA, NA_character_)
pr = parse_recode_instructions(i, catch_NAs = TRUE, simplify_to_numeric = TRUE)

test_that('Recodes get properly parsed',{
  expect_equal(r1, pr[[1]])
  expect_equal(r2, pr[[2]])
  expect_equal(r3, pr[[3]])
  expect_equal(r4, pr[[4]])
  expect_equal(r5, pr[[5]])
})

test_that('Recodes behave as expected',{
  start = data.table(a1 = 1:3, a2 = 1:3, a3 = 1:3, a4 = 1:3, a5 = 7:9)
  end = data.table(b1 = 4:6, b2 = factor(4:6, 4:6, c('a','b','c')), a3 = 4:6, b4 = 4:6, b5 = 7:9)
  res = enact_recodes(start, pr)
  expect_equal(res[, .(b1, b2, a3, b4, b5)], end )
})
