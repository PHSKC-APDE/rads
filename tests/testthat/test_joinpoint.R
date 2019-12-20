library('data.table')
library('dplyr')
library('testthat')

# load data
dt <- data.table(
  indic = rep(c(rep("indicator#1", 7), rep("indicator#2", 7)), 2), 
  group1 = c(rep("Female", 14), rep("Male", 14)), 
  group2 = rep(c(rep("Child", 7), rep("Adult", 7)), 2),
  time = c(rep(2001:2007, 4)), 
  est = c(.11, .12, .13, .14, .15, .16, .17, # indicator#1, female, child ... rise 1-7
          .29, .48, .33, .45, .12, .17, .55, # indiator#2, female, adult ... noisy ... so flat
          .77, .67, .57, .40, .50, .60, .70, # indicator#1, male, child ... fall 1-4, rise 4-7
          .35, .33, .31, .29, .27, .25, .23), # indicator#2, male, adult ... fall 1-7
  se = c(runif(28, .01, .02))
)

# set temporary folder
my.dir <- "C:/temp/jp_test_data/"

# run JoinPoint
jp.output <- jp_f(jp_data = dt,
                  jp_indicator = "indic",
                  jp_period = "time",
                  jp_result = "est",
                  jp_se = "se",
                  jp_byvar1 = "group1",
                  jp_byvar2 = "group2",
                  jp_dir = my.dir
) 

# run tests
test_that('Check joinpoint summary data',{
  expect_equal( jp.output[indic=="indicator#1" & group1=="Female" & group2 == "Child"]$time_trends, "2001-07: rising")
  expect_equal( jp.output[indic=="indicator#2" & group1=="Female" & group2 == "Adult"]$time_trends, "2001-07: flat")
  expect_equal( jp.output[indic=="indicator#1" & group1=="Male" & group2 == "Child"]$time_trends, "2001-04: falling; 2004-07: rising")
  expect_equal( jp.output[indic=="indicator#2" & group1=="Male" & group2 == "Adult"]$time_trends, "2001-07: falling")
})

test_that('Check joinpoint input data were saved',{
  expect_equal(file.exists(paste0(my.dir, '/input/JP_indicator#1.txt')), TRUE)
  expect_equal(file.exists(paste0(my.dir, '/input/JP_indicator#2.txt')), TRUE)
})

test_that('Check joinpoint output data were saved',{
  expect_equal(file.exists(paste0(my.dir, '/output/indicator#1.jpo')), TRUE)
  expect_equal(file.exists(paste0(my.dir, '/output/indicator#2.jpo')), TRUE)
})
