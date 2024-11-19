library(testthat)


test_that("linear regression function tests", {
  model=lm(mpg~cyl+disp+hp,mtcars)
  #test if output is a list
  result=linear_regression_function(mtcars, "mpg", c("cyl", "disp", "hp"))
  expect_type(result, "list")

  #test coefficients
  for (i in 1:4) {
    expect_equal(summary(model)$coefficients[i,1],
                 result$Coefficients$Estimate[i],
                 tolerance = 1e-5)
    expect_equal(summary(model)$coefficients[i,2],
                 result$Coefficients$Std_Error[i],
                 tolerance = 1e-5)
    expect_equal(summary(model)$coefficients[i,3],
                 result$Coefficients$t_value[i],
                 tolerance = 1e-5)
    expect_equal(summary(model)$coefficients[i,4],
                 result$Coefficients$p_value[i],
                 tolerance = 1e-5)
  }

  #test confidence interval
  CI=confint(model)
  for (i in 1:4) {
    expect_equal(CI[i,1],
                 result$Coefficients$CI_lower[i],
                 tolerance = 1e-5)
    expect_equal(CI[i,2],
                 result$Coefficients$CI_upper[i],
                 tolerance = 1e-5)
  }

  #test R_squared
  expect_equal(summary(model)$r.squared,
               result$Multiple_R_squared,
               tolerance = 1e-5)

  #test adjusted_R_squared
  expect_equal(summary(model)$adj.r.squared ,
               result$Adjusted_R_squared,
               tolerance = 1e-5)

  #test residuals
  expect_equal(as.numeric(quantile(residuals(model))[1]),
               as.numeric(result$Residuals[1]),
               tolerance = 1e-5)
  expect_equal(as.numeric(quantile(residuals(model))[2]),
               as.numeric(result$Residuals[2]),
               tolerance = 1e-5)
  expect_equal(as.numeric(quantile(residuals(model))[3]),
               as.numeric(result$Residuals[3]),
               tolerance = 1e-5)
  expect_equal(as.numeric(quantile(residuals(model))[4]),
               as.numeric(result$Residuals[4]),
               tolerance = 1e-5)
  expect_equal(as.numeric(quantile(residuals(model))[5]),
               as.numeric(result$Residuals[5]),
               tolerance = 1e-5)
})
