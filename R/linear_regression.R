#' Linear Regression
#'
#' Implements a simple linear regression algorithm.
#'
#' @param formula A formula specifying the model (e.g., y ~ x).
#' @param data A data frame containing the variables in the model.
#' @return A list containing coefficients, fitted values, residuals, and R-squared.
#' @examples
#' model <- linear_regression(mpg ~ wt, data = mtcars)
#' model
#' @export
linear_regression <- function(formula, data) {
  # Extract response and predictor variables
  model_frame <- model.frame(formula, data)
  y <- model.response(model_frame)
  X <- model.matrix(attr(model_frame, "terms"), model_frame)

  # Calculate coefficients using OLS: (X'X)^(-1) X'y
  beta <- solve(t(X) %*% X) %*% t(X) %*% y

  # Fitted values and residuals
  fitted_values <- X %*% beta
  residuals <- y - fitted_values

  # R-squared
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)

  # Return result as a list
  list(
    coefficients = beta,
    fitted_values = fitted_values,
    residuals = residuals,
    r_squared = r_squared
  )
}
