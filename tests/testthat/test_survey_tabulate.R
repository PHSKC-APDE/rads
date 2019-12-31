# library('srvyr')
# library('survey')
# library('dplyr')
#
# data(api) #from the survey package
# sur = apisrs %>% as_survey_design(ids = 1, fpc = fpc)
#
# test_that('Defaults work: svy',
#           expect_equal(
#             survey_tabulate(sur, what = 'api00')$mean,
#             sur %>% summarize(mean = survey_mean(api00, na.rm = T)) %>% .$mean
#           ))
#
# test_that('Grouping without filtering',
#           expect_equal(
#             survey_tabulate(sur, 'api00', by = c('stype'), metrics = 'denominator')[, .(stype, denominator)],
#             sur %>% group_by(stype) %>% summarize(denominator = unweighted(n())) %>% setDT
#           ))
#
# test_that('Multi Grouping with filtering',{
#           st = survey_tabulate(sur, 'api00', cname == 'Los Angeles', by = c('stype', 'cname'), metrics = 'denominator')[, .(stype, cname, denominator)]
#           man = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(denominator = unweighted(n())) %>% setDT
#           expect_equal(st, man)
# })
#
# test_that('Multi Grouping with filtering and multiple what variables',{
#   st = survey_tabulate(sur, what = c('api00', 'api99'),
#                        cname == 'Los Angeles',
#                        by = c('stype', 'cname'),
#                        metrics = 'mean')
#   man1 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api00)) %>% setDT
#   man2 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api99)) %>% setDT
#   man = rbind(man1, man2)[, .(stype, cname, mean, variable = c(rep('api00', 3), rep('api99', 3)))]
#   expect_equal(st, man)
# })
#
# test_that('Proportion toggle- on',{
#   sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
#   st = survey_tabulate(sur, 'schwide', metrics = 'lower', proportion = T)
#   man = sur %>% summarize(blah = survey_mean(schwide, vartype = 'ci', proportion = T)) %>% setDT
#
#   #without proportion
#   expect_equal(st[, lower], man[, blah_low])
#
# })
#
# test_that('Proportion toggle changes result',{
#   sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
#   a = survey_tabulate(sur, 'schwide', metrics = 'lower', proportion = T)[, lower]
#   b = survey_tabulate(sur, 'schwide', metrics = 'lower', proportion = F)[, lower]
#
#   expect_true(a != b)
#
# })
#
# test_that('Invalid input for svy',{
#   expect_error(survey_tabulate('a', 'api00'), 'inherits.')
# })
#
# test_that('Invalid input for what',{
#   expect_error(survey_tabulate(sur, 'thisvarnoexists'), 'thisvarnoexists')
# })
#
# test_that('Invalid input for ... filters',{
#   expect_error(survey_tabulate(sur, 'api00', bananas>100, api00 < 50), 'bananas')
# })
#
# test_that('Invalid input for by',{
#   expect_error(survey_tabulate(sur, 'api00', by = 'turtles'), 'turtles')
# })
#
# test_that('Invalid input for metric',{
#   expect_error(survey_tabulate(sur, 'api00', metrics = 'turtles'), 'should be one of')
# })
#
