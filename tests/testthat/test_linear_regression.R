library(testthat)
library(linreg)

test_that("linear_regression calculates coefficients correctly", {
  model <- linear_regression(mpg ~ wt, data = mtcars)
  expect_length(model$coefficients, 2)
  expect_true(model$r_squared > 0)
})

test_that("linear_regression handles edge cases", {
  data <- data.frame(y = c(1, 2, 3), x = c(1, 2, 3))
  model <- linear_regression(y ~ x, data = data)
  expect_equal(model$r_squared, 1)
})
