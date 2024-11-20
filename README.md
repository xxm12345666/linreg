
# linreg

`linreg` is an R package that implements linear regression without using the `lm` function.


## Installation

To install the development version of `linreg` from GitHub, use:

```{r}
# Install the devtools package if not installed
install.packages("devtools")

# Install linreg from GitHub
devtools::install_github("xxm12345666/linreg")
```
Usage

Here is an example of how to use linreg:

```{r}
library(linreg)
# Fit a linear regression model
model <- linear_regression(mpg ~ wt, data = mtcars)

# Print the model summary
print(model)
```

