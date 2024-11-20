library(testthat)
library(linreg)

# Test single predictor regression
test_that("linear_regression calculates correct coefficients for single predictor", {
  # Fit the model with linreg
  model <- linear_regression(mpg ~ wt, data = mtcars)

  # Fit the model with lm for comparison
  lm_model <- lm(mpg ~ wt, data = mtcars)

  # Compare coefficients
  expect_equal(model$coefficients, as.vector(coef(lm_model)), tolerance = 1e-5)

  # Compare R-squared
  expect_equal(model$r_squared, summary(lm_model)$r.squared, tolerance = 1e-5)

  # Compare adjusted R-squared
  expect_equal(model$adj_r_squared, summary(lm_model)$adj.r.squared, tolerance = 1e-5)
})

# Test multiple predictors regression
test_that("linear_regression handles multiple predictors correctly", {
  # Fit the model with linreg
  model <- linear_regression(mpg ~ wt + hp, data = mtcars)

  # Fit the model with lm for comparison
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)

  # Compare coefficients
  expect_equal(model$coefficients, as.vector(coef(lm_model)), tolerance = 1e-5)

  # Compare R-squared
  expect_equal(model$r_squared, summary(lm_model)$r.squared, tolerance = 1e-5)

  # Compare adjusted R-squared
  expect_equal(model$adj_r_squared, summary(lm_model)$adj.r.squared, tolerance = 1e-5)
})

# Test residuals
test_that("linear_regression calculates residuals correctly", {
  # Fit the model with linreg
  model <- linear_regression(mpg ~ wt, data = mtcars)

  # Fit the model with lm for comparison
  lm_model <- lm(mpg ~ wt, data = mtcars)

  # Compare residuals
  expect_equal(model$residuals, as.vector(residuals(lm_model)), tolerance = 1e-5)
})

# Test p-values
test_that("linear_regression calculates p-values correctly", {
  model <- linear_regression(mpg ~ wt, data = mtcars)
  lm_model <- lm(mpg ~ wt, data = mtcars)
  lm_summary <- summary(lm_model)
})



# Test standard errors
test_that("linear_regression calculates standard errors correctly", {
  # Fit the model with linreg
  model <- linear_regression(mpg ~ wt, data = mtcars)

  # Fit the model with lm for comparison
  lm_model <- lm(mpg ~ wt, data = mtcars)
  lm_summary <- summary(lm_model)

  # Compare standard errors
  expect_equal(model$std_error, coef(lm_summary)[, "Std. Error"], tolerance = 1e-5)
})

# Test edge cases
test_that("linear_regression handles edge cases correctly", {
  # Single data point
  single_data <- data.frame(mpg = 25, wt = 3)
  expect_error(
    linear_regression(mpg ~ wt, data = single_data),
    "The design matrix is singular and cannot be inverted"
  )


  # Missing data
  missing_data <- mtcars
  missing_data$mpg[1] <- NA
  expect_warning(linear_regression(mpg ~ wt, data = missing_data, na.action = na.pass), "missing values")

  # Perfect fit
  perfect_fit_data <- data.frame(y = c(1, 2, 3), x = c(1, 2, 3))
  model <- linear_regression(y ~ x, data = perfect_fit_data)
  expect_equal(model$r_squared, 1)
})

test_that("linear_regression handles missing values correctly", {
  missing_data <- mtcars
  missing_data$mpg[1] <- NA

  # Expect warning but continue execution
  expect_warning(
    model <- linear_regression(mpg ~ wt, data = missing_data, na.action = na.pass),
    "Data contains missing values"
  )

  # Ensure residuals are calculated
  expect_true(!any(is.na(model$residuals)))
})
