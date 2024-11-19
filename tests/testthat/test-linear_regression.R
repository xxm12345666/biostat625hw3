test_that("multiplication works", {
  fit <- linear_regression(mpg ~ wt + hp, data = mtcars)
  lm_fit <- lm(mpg ~ wt + hp, data = mtcars)

  expect_equal(fit$coefficients, coef(lm_fit), tolerance = 1e-8)
  expect_equal(length(fit$residuals), nrow(mtcars))
})
