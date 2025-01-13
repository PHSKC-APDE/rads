library('testthat')
library('rads')
library('data.table')
library('dtsurvey')
library('survey')
set.seed(98104)
data(api)
apiclus1$ysw = as.numeric(apiclus1$sch.wide == "Yes")
midat = lapply(1:10, function(x){
  r = apiclus1

  # Some random group assignments
  r$random = sample(1:5, nrow(r), T)
  r$random3 = sample(1:3, nrow(r), T)
  r$random_fact = factor(r$random)

  # Make some variables to test various edge cases
  # Test what happens when there is no variation within a group
  r$ysw_test_novar = r$ysw
  if(x %in% c(1,4)) r$ysw_test_novar[r$random3 == 1] <- 0 # all 0 in random3 subgroup 1 for iterations 1 and 4

  ## The target variable is all the same in a given grouper
  ## This will be a double group by
  r$stype_test_novar = r$stype
  if(x %in% c(2,5)) r$stype_test_novar[r$random3 == 1] <- 'E' # all E in random3 subgroup 1 for iterations 1 and 4
  r$m_bin = as.numeric(r$stype_test_novar == 'M')
  r$m_bin2 = as.numeric(r$stype == 'M')
  # Test what happens where there is no combination within a group
  dtsurvey(r, 'dnum', weight = 'pw')

})

# store them in a imputationList (so the s3 calc method will be called)
midat = mitools::imputationList(midat)

r13.2 = calc(midat, 'stype_test_novar', by = c('random3'), metrics = c('mean', 'numerator'))
r13.3 = calc(midat, 'm_bin', by = c('random3'), metrics = c('mean', 'numerator'))
r13.2[level == 'M']
r13.3

r13.4 = calc(midat, 'stype', by = 'random', metrics = c('mean', 'numerator'))[level == 'M']
r13.5 = calc(midat, 'm_bin2', by = 'random', metrics = 'mean')
r13.4
r13.5

# Open questions
# Certain 13.2 iterations have an all 0 vcov matrix that gets summarized with more legit ones
# Should we expect 13.2 and 13.3 to be equal the way r13.4 and 13.5 are?
# What should not mi imputed metrics be filled with. Right now its mean over the iterations with na.rm = F
