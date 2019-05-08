context("Check Functions Testing")
library(binomial)

test_that("check_prob produces error for prob < 0 and prob > 1", {
  expect_error(check_prob(-.2))
  expect_error(check_prob(1.2))
})

test_that("check_prob takes probabilities of length 1", {
  expect_length(check_prob(.2), 1)
  expect_error(check_prob(c(.2, .5)))
})

test_that("check_prob produces logical output", {
  expect_equal(class(check_prob(.1)), 'logical')
})

test_that("check_trials produces error negative numbers", {
  expect_error(check_trials(-2))
})

test_that("check_trials produces error for non-integers", {
  expect_error(check_trials(1.2))
})

test_that("check_trials produces logical output", {
  expect_equal(class(check_trials(5)), 'logical')
})

test_that("check_success with vector of success less than trials is valid", {
  expect_equal(check_success(1:5, 10), TRUE)
})

test_that("check_success produces error for non-integers", {
  expect_error(check_success(1.2, 5))
})

test_that("check_success produces logical output", {
  expect_equal(class(check_success(5, 10)), 'logical')
})