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
  # Validate inputs
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!is.numeric(y)) stop("y must be a numeric vector")
  if (nrow(X) != length(y)) stop("Number of rows in X must match the length of y.")

  # Add intercept column
  X <- cbind(Intercept = 1, X)

  # Compute X'X
  XtX <- t(X) %*% X

  # Check for singularity
  if (kappa(XtX) > 1e12) stop("The matrix X'X is ill-conditioned or singular. Check for collinearity.")

  # Solve for coefficients
  XtX_inv <- solve(XtX)
  coefficients <- XtX_inv %*% t(X) %*% y

  # Calculate fitted values and residuals
  fitted_values <- X %*% coefficients
  residuals <- y - fitted_values

  # Residual sum of squares (RSS) and total sum of squares (TSS)
  RSS <- sum(residuals^2)
  TSS <- sum((y - mean(y))^2)

  # R-squared and Residual Standard Error (RSE)
  R2 <- 1 - RSS / TSS
  n <- nrow(X)
  p <- ncol(X)
  RSE <- sqrt(RSS / (n - p))

  # Compute standard errors, t-values, and p-values
  if (ncol(X) == 1) {  # Handle intercept-only model
    SE <- t_values <- p_values <- NA
  } else {
    sigma_squared <- RSS / (n - p)
    SE <- sqrt(diag(XtX_inv) * sigma_squared)
    t_values <- coefficients / SE
    p_values <- 2 * pt(-abs(t_values), df = n - p)
  }

  # Output results
  result <- list(
    coefficients = as.vector(coefficients),
    SE = as.vector(SE),
    t_values = as.vector(t_values),
    p_values = as.vector(p_values),
    fitted_values = as.vector(fitted_values),
    residuals = as.vector(residuals),
    RSS = RSS,
    R2 = R2,
    RSE = RSE
  )

  # Assign class to the result
  class(result) <- "linear_regression"
  return(result)
}

#' Print Linear Regression Results
#'
#' @param x An object of class linear_regression.
#' @param ... Additional arguments (currently ignored).
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
