

# linreg

![R-CMD-check](https://github.com/xxm12345666/linreg/actions/workflows/test-coverage.yml/badge.svg)
[![codecov](https://codecov.io/gh/xxm12345666/linreg/branch/main/graph/badge.svg)](https://codecov.io/gh/YourGitHubUsername/linreg)


`linreg` is an R package that implements linear regression without using the `lm` function.


## Installation

To install the development version of `linreg` from GitHub, use:

```{r}
# Install the devtools package if not installed
install.packages("devtools")

# Install linreg from GitHub
devtools::install_github("xxm12345666/linreg")
```
## Single Predictor Model

Perform a simple linear regression with one predictor (wt) and a dependent variable (mpg):

```{r}
library(linreg)

# Perform linear regression
model <- linear_regression(mpg ~ wt, data = mtcars)

# Print the model summary
print(model)
```
The model object includes:

- coefficients: Intercept and slope(s).
- r_squared: R-squared value.
- adj_r_squared: Adjusted R-squared value.
- residuals: Residuals from the model.
- std_error: Standard errors for the coefficients.
- t_values: t-statistics for the coefficients.
- p_values: p-values for the coefficients.

## Multiple Predictors Model

You can include multiple predictors to build a more complex model:
```{r}
# Perform linear regression with multiple predictors
model <- linear_regression(mpg ~ wt + hp, data = mtcars)

# Print the model summary
print(model)
```

## Comparing linear_regression and lm
The linear_regression function replicates many features of lm. Below is an example of how to compare results:

```{r}
# Fit models using linreg and lm
linreg_model <- linear_regression(mpg ~ wt, data = mtcars)
lm_model <- lm(mpg ~ wt, data = mtcars)

# Compare coefficients
cat("linreg coefficients:", linreg_model$coefficients, "\n")
cat("lm coefficients:", coef(lm_model), "\n")

# Compare R-squared values
cat("linreg R-squared:", linreg_model$r_squared, "\n")
cat("lm R-squared:", summary(lm_model)$r.squared, "\n")
```

## Residual Diagnostics

You can visualize residuals from the model to evaluate its fit:

```{r}
# Plot residuals
plot(linreg_model$residuals, main = "Residuals (linreg)", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)
```


