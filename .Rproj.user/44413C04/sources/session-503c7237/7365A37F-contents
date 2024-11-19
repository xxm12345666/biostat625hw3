#' Custom Linear Regression Function
#'
#' Fits a linear regression model, supporting multiple variables, and provides coefficient estimates, residuals, R square, and other statistical metrics.
#'
#' @importFrom stats pt
#' @param X A matrix or data frame containing all predictor variables.
#' @param y A numeric vector of the response variable.
#' @return A list containing coefficients, residuals, standard errors, R square, and other metrics.
#' @examples
#' # Example with a single predictor
#' X <- matrix(c(1, 2, 3, 4), ncol = 1)
#' y <- c(2, 4, 6, 8)
#' result <- linear_regression(X, y)
#' print(result)
#'
#' # Example with multiple predictors
#' X <- matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 2)
#' y <- c(3, 6, 7, 9)
#' result <- linear_regression(X, y)
#' print(result)
#' @export
linear_regression <- function(X, y) {
  # Ensure X is a matrix
  if (!is.matrix(X)) {
    if (is.data.frame(X)) {
      X <- as.matrix(X)
    } else {
      stop("X must be a matrix or data frame")
    }
  }

  # Ensure y is a numeric vector
  if (!is.vector(y)) {
    stop("y must be a numeric vector")
  }

  # Add intercept column
  X <- cbind(Intercept = 1, X)

  # Solve for regression coefficients using the least squares formula
  XtX_inv <- solve(t(X) %*% X)  # Compute (X'X)^(-1)
  XtY <- t(X) %*% y             # Compute X'Y
  coefficients <- XtX_inv %*% XtY

  # Compute fitted values
  fitted_values <- X %*% coefficients

  # Compute residuals
  residuals <- y - fitted_values

  # Compute residual sum of squares (RSS)
  RSS <- sum(residuals^2)

  # Compute total sum of squares (TSS)
  TSS <- sum((y - mean(y))^2)

  # Compute R-squared
  R2 <- 1 - RSS / TSS

  # Number of observations and predictors
  n <- nrow(X)
  p <- ncol(X)

  # Residual standard error (RSE)
  RSE <- sqrt(RSS / (n - p))

  # Compute standard errors of coefficients
  sigma_squared <- RSS / (n - p)
  SE <- sqrt(diag(XtX_inv) * sigma_squared)

  # Compute t-values and p-values
  t_values <- coefficients / SE
  p_values <- 2 * pt(-abs(t_values), df = n - p)

  # Output results
  result <- list(
    coefficients = as.vector(coefficients),       # Regression coefficients
    SE = as.vector(SE),                           # Standard errors
    t_values = as.vector(t_values),               # t-values
    p_values = as.vector(p_values),               # p-values
    fitted_values = as.vector(fitted_values),     # Fitted values
    residuals = as.vector(residuals),             # Residuals
    RSS = RSS,                                    # Residual sum of squares
    R2 = R2,                                      # R-squared
    RSE = RSE                                     # Residual standard error
  )

  # Assign class to the result
  class(result) <- "linear_regression"
  return(result)
}

#' Print Linear Regression Results
#'
#' @param x An object of class linear_regression.
#' @export
print.linear_regression <- function(x, ...) {
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("\nStandard Errors:\n")
  print(x$SE)
  cat("\nt values:\n")
  print(x$t_values)
  cat("\np values:\n")
  print(x$p_values)
  cat("\nResidual standard error (RSE):", x$RSE, "\n")
  cat("R-squared:", x$R2, "\n")
}
