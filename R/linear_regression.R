#' Linear Regression
#'
#' A function that implements linear regression without using `lm`.
#'
#' @param formula A formula specifying the model (e.g., y ~ x).
#' @param data A data frame containing the variables in the model.
#' @param na.action A function indicating what should happen when the data contains NAs. Default is `na.omit`.
#' @importFrom stats model.frame model.response model.matrix na.omit
#' @return A list containing:
#'   \describe{
#'     \item{coefficients}{Estimated coefficients.}
#'     \item{fitted_values}{Fitted values.}
#'     \item{residuals}{Residuals from the model.}
#'     \item{r_squared}{R-squared value of the model.}
#'     \item{adj_r_squared}{Adjusted R-squared value of the model.}
#'     \item{std_error}{Standard errors of the coefficients.}
#'     \item{t_values}{t-statistics for the coefficients.}
#'     \item{p_values}{p-values for the coefficients.}
#'     \item{df_residual}{Degrees of freedom for residuals.}
#'   }
#' @examples
#' # Example using mtcars dataset
#' model <- linear_regression(mpg ~ wt + hp, data = mtcars)
#' print(model)
#' @export
linear_regression <- function(formula, data, na.action = na.omit) {
  # Handle NA values
  data <- na.action(data)

  # Extract response and predictor variables
  model_frame <- model.frame(formula, data)
  y <- model.response(model_frame)
  X <- model.matrix(attr(model_frame, "terms"), model_frame)

  # Calculate coefficients using OLS
  beta <- solve(t(X) %*% X) %*% t(X) %*% y

  # Fitted values and residuals
  fitted_values <- X %*% beta
  residuals <- y - fitted_values

  # Degrees of freedom
  n <- nrow(X)
  p <- ncol(X)
  df_residual <- n - p

  # Residual standard error
  sigma_squared <- sum(residuals^2) / df_residual
  sigma <- sqrt(sigma_squared)

  # Variance-covariance matrix
  cov_matrix <- sigma_squared * solve(t(X) %*% X)

  # Standard errors of coefficients
  std_error <- sqrt(diag(cov_matrix))

  # t-values and p-values
  t_values <- beta / std_error
  p_values <- 2 * pt(-abs(t_values), df = df_residual)

  # R-squared and Adjusted R-squared
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / df_residual)

  # Return result as a list
  list(
    coefficients = as.vector(beta),
    fitted_values = as.vector(fitted_values),
    residuals = as.vector(residuals),
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    std_error = std_error,
    t_values = as.vector(t_values),
    p_values = as.vector(p_values),
    df_residual = df_residual
  )
}
