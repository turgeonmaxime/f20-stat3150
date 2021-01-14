## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

knitr::opts_chunk$set(cache = FALSE, message = FALSE,
                      linewidth = 50)


## -----------------------------------------------------------------------------
library(boot)

log_lik_der <- function(lambda) {
    n <- nrow(aircondit)
    n/lambda - sum(aircondit$hours) 
}

# We will look for a solution on [0.001, 1]
# We found the bounds by trial and error
uniroot(log_lik_der, 
        c(0.001, 1))

# Check whether we get the same value
# as analytical solution
1/mean(aircondit$hours)


## -----------------------------------------------------------------------------
# Create three vectors:
# 1. Lower bounds of bins
# 2. Upper bounds of bins
# 3. Number of values in bins
a_vec <- c(0, 2, 3, 4, 5, 6)
b_vec <- c(2, 3, 4, 5, 6, Inf)
n_vec <- c(2, 3, 1, 2, 1, 1)


## -----------------------------------------------------------------------------
log_lik_der_binned <- function(lambda) {
    num <- n_vec*(-a_vec*exp(-lambda*a_vec) +
                    b_vec*exp(-lambda*b_vec))
    # Need to fix last value manually
    # to avoid NaN value
    num[6] <- -n_vec[6]*a_vec[6]*exp(-lambda*a_vec[6])
    denom <- exp(-lambda*a_vec) - exp(-lambda*b_vec)
    sum(num/denom)
}


## -----------------------------------------------------------------------------
# We will look for a solution on [0.1, 1]
uniroot(log_lik_der_binned, 
        c(0.1, 1))

