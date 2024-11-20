#' Print Linear Regression Results
#'
#' This function prints a summary of the results from the linear_regression function.
#'
#' @param x An object of class linear_regression.
#' @param ... Additional arguments (not used).
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
