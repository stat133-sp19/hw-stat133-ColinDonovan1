context("Auxiliary Function Testing")
library(binomial)

test_that("aux_mean gives mean", {
  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_equal(aux_mean(10, 0.2), 2)
})

test_that("aux_mean gives output of length 1", {
  expect_length(aux_mean(5, 0.5), 1)
  expect_length(aux_mean(10, 0.2), 1)
})

test_that("aux_kurtosis gives kurtosis", {
  expect_equal(aux_kurtosis(15, 0.3), -0.0825397)
  expect_equal(aux_kurtosis(10, 0.4), -0.1833333)
  expect_equal(aux_kurtosis(10, 0.8), 0.025)
})

test_that("aux_mode gives mode", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(5, 0.5), c(3, 2))
})

test_that("aux_variance gives variance", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(15, 0.5), 3.75)
  expect_equal(aux_variance(10, 0.5), 2.5)
})

test_that("aux_skewness gives skewness", {
  expect_equal(aux_skewness(10, 0.4), 0.1290994)
  expect_equal(aux_skewness(10, 0.5), 0)
  expect_equal(aux_skewness(15, 0.2), 0.3872983)
})