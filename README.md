

# linreg

<!-- badges: start -->
![R-CMD-check](https://github.com/xxm12345666/linreg/actions/workflows/test-coverage.yml/badge.svg)(https://github.com/xxm12345666/linreg/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/xxm12345666/linreg/branch/main/graph/badge.svg)](https://codecov.io/gh/xxm12345666/linreg)
<!-- badges: end -->

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

Here's an example using the built-in mtcars dataset to predict miles per gallon (mpg) based on the number of cylinders (cyl), displacement (disp), and horsepower (hp):

```{r}
library(linreg)

# Perform linear regression
data(mtcars)
result <- linear_regression(mtcars, "mpg", c("cyl", "disp", "hp"))

# View results
print(result)

```
## Output Details
The result is a list containing the following components:

Coefficients: A table with estimated coefficients, standard errors, t-values, and p-values.
Residuals: A summary of the residuals (min, Q1, median, Q3, max).
Residual Standard Error: The standard deviation of residuals.
R-squared and Adjusted R-squared: Metrics indicating the proportion of variability explained by the model.
F-statistic: A test for the overall significance of the model.

![Image Description](/Users/maggiexu/Desktop/1.png)
