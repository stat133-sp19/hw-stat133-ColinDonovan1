context("Binomial Functions Testing")
library(binomial)

test_that("bin_choose can take vectorized input", {
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
  expect_equal(bin_choose(10, 1:5), c(10, 45, 120, 210, 252))
})

test_that("bin_c produces error with k > n", {
  expect_equal(bin_choose(5, 6), "n must be greater than k")
  expect_warning(bin_choose(5, 1:10))
})

test_that("bin_choose correctly calculated coefficient", {
  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(20, 4), 4845)
})

test_that("bin_probability can take vectorized input for success", {
  expect_equal(bin_probability(0:3, 5, 0.3), c(0.16807, 0.36015, 0.30870, 0.13230))
})

test_that("bin_probability produces error with successes > trials", {
  expect_error(bin_probability(6, 5, 0.3))
})

test_that("bin_probability gives correct length", {
  expect_length(bin_probability(0:5, 10, .3), 6)
  expect_length(bin_probability(5, 10, 0.3), 1)
})

test_that("bin_distribution output with correct class", {
  expect_equal(class(bin_distribution(5, 0.3)), c('bindis', 'data.frame'))
})

test_that("bin_distribution produces error with incorrect probability value", {
  expect_error(bin_distribution(5, 1.2))
})

test_that("bin_distribution gives correct number of rows (trials + 1)", {
  expect_equal(nrow(bin_distribution(5, .3)), 6)
})

test_that("bin_cumulative has cumulative end at 1", {
  expect_equal(bin_cumulative(5, 0.3)[6, 3], 1)
})

test_that("bin_cumulative gives correct number of rows (trials + 1)", {
  expect_equal(nrow(bin_cumulative(5, 0.3)), 6)
})

test_that("bin_cumulative gives correct number of columns", {
  expect_equal(ncol(bin_cumulative(5, .3)), 3)
})