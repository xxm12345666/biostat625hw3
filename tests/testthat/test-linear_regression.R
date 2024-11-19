library(testthat)

test_that("linear regression function tests", {
  # Fit models using custom function and lm()
  result = linear_regression(mpg ~ cyl + disp + hp, data = mtcars)
  model = lm(mpg ~ cyl + disp + hp, data = mtcars)

  # Test if output is a list
  expect_type(result, "list")

  # Test coefficients
  expect_equal(result$coefficients, coef(model), tolerance = 1e-5)

  # Test confidence intervals
  CI = confint(model)
  for (i in seq_along(result$coefficients)) {
    expect_equal(CI[i, 1], result$CI_lower[i], tolerance = 1e-5)
    expect_equal(CI[i, 2], result$CI_upper[i], tolerance = 1e-5)
  }

  # Test R-squared
  expect_equal(result$R2, summary(model)$r.squared, tolerance = 1e-5)

  # Test adjusted R-squared
  expect_equal(result$Adjusted_R_squared, summary(model)$adj.r.squared, tolerance = 1e-5)

  # Test residuals
  expect_equal(result$residuals, residuals(model), tolerance = 1e-5)
})
