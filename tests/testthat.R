# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(LinearRegression)

test_that("linear_regression works", {
  X <- matrix(c(1, 2, 3, 4), ncol = 1)
  y <- c(2, 4, 6, 8)
  result <- linear_regression(X, y)
  expect_equal(as.vector(result$coefficients), c(0, 2))
})
