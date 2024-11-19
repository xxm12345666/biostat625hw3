#' Linear Regression Function
#'
#' This function performs a linear regression and calculates data related to linear regression models.
#'
#' @param formula A formula specifying the model (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing the variables used in the formula.
#' @return A list with the following components:
#' \itemize{
#'   \item \code{coefficients}: The estimated regression coefficients (named vector).
#'   \item \code{SE}: The standard errors of the coefficients.
#'   \item \code{t_values}: The t-statistics for the coefficients.
#'   \item \code{p_values}: The p-values for the coefficients.
#'   \item \code{fitted_values}: The predicted values from the model.
#'   \item \code{residuals}: The residuals from the model.
#'   \item \code{RSS}: Residual sum of squares.
#'   \item \code{R2}: R-squared statistic.
#'   \item \code{RSE}: Residual standard error.
#' }
#' @export
#' @examples
#' data(mtcars)
#' result <- linear_regression(mpg ~ wt + hp, data = mtcars)
#' print(result)
linear_regression <- function(formula, data) {
  # Validate inputs
  if (missing(data)) stop("Data must be provided when using a formula.")
  if (!inherits(formula, "formula")) stop("The first argument must be a valid formula (e.g., y ~ x1 + x2).")

  # Extract model frame
  mf <- model.frame(formula, data)
  y <- model.response(mf)  # Response variable
  X <- model.matrix(attr(mf, "terms"), mf)  # Predictor matrix

  # Compute X'X and check for singularity
  XtX <- t(X) %*% X
  if (kappa(XtX) > 1e12) {
    stop("The matrix X'X is singular or nearly singular. This could be due to collinearity among predictors.")
  }

  # Solve for coefficients
  XtX_inv <- solve(XtX)
  coefficients <- XtX_inv %*% t(X) %*% y

  # Add names to coefficients
  coef_names <- colnames(X)
  names(coefficients) <- coef_names

  # Calculate fitted values and residuals
  fitted_values <- X %*% coefficients
  residuals <- y - fitted_values

  # Compute residual sum of squares (RSS) and total sum of squares (TSS)
  RSS <- sum(residuals^2)
  TSS <- sum((y - mean(y))^2)

  # Compute R-squared and Residual Standard Error (RSE)
  R2 <- 1 - RSS / TSS  # Proportion of variance explained by the model
  n <- nrow(X)
  p <- ncol(X)
  RSE <- sqrt(RSS / (n - p))  # Estimate of the standard deviation of residuals

  # Compute standard errors, t-values, and p-values
  sigma_squared <- RSS / (n - p)
  SE <- sqrt(diag(XtX_inv) * sigma_squared)
  t_values <- coefficients / SE
  p_values <- 2 * pt(-abs(t_values), df = n - p)

  # Add names to SE, t-values, and p-values
  names(SE) <- coef_names
  names(t_values) <- coef_names
  names(p_values) <- coef_names

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
