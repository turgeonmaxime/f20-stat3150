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
set.seed(3150)


## -----------------------------------------------------------------------------
# Generate a random sample
n <- 20
xvars <- rgamma(n, shape = 3, rate = 5.5)

# Compute the estimate
theta_hat <- mean((xvars - mean(xvars))^2)
c("estimate" = theta_hat,
  "theoretical" = 3/5.5^2)


## -----------------------------------------------------------------------------
# Jackknife
theta_i_hat <- numeric(n)

for (i in 1:n) {
  xvars_jack <- xvars[-i]
  mean_i <- mean(xvars_jack)
  theta_i_hat[i] <- mean((xvars_jack - mean_i)^2)
}


## -----------------------------------------------------------------------------
# Estimate of bias
(bias <- (n-1)*(mean(theta_i_hat) - theta_hat))


c("De-biased" = theta_hat - bias,
  "Unbiased" = var(xvars))


## ---- linewidth = 75----------------------------------------------------------
library(bootstrap)
str(patch)


## -----------------------------------------------------------------------------
# Estimate of theta
theta_hat <- mean(patch$y)/mean(patch$z)


## -----------------------------------------------------------------------------
# Jackknife
n <- nrow(patch)
theta_i <- numeric(n)

for (i in 1:n) {
  theta_i[i] <- mean(patch[-i,"y"])/mean(patch[-i,"z"])
}


## -----------------------------------------------------------------------------
# Estimate of bias
(bias <- (n-1)*(mean(theta_i) - theta_hat))

c("Biased" = theta_hat,
  "De-biased" = theta_hat - bias)


## -----------------------------------------------------------------------------
# NOT THE SAME THING
mean(patch$y/patch$z)


## -----------------------------------------------------------------------------
# Continuing on with the patch dataset
(se <- sqrt((n-1)*mean((theta_i - mean(theta_i))^2)))
# 95% CI
c("LB" = theta_hat - bias - 1.96*se,
  "UB" = theta_hat - bias + 1.96*se)


## -----------------------------------------------------------------------------
library(bootstrap)
str(law)


## -----------------------------------------------------------------------------
# Estimate of rho
(rho_hat <- cor(law$LSAT, law$GPA))


## -----------------------------------------------------------------------------
# Jackknife
n <- nrow(law)
rho_i <- numeric(n)

for (i in 1:n) {
  rho_i[i] <- cor(law$LSAT[-i], law$GPA[-i])
}


## -----------------------------------------------------------------------------
# Estimate of bias
(bias <- (n-1)*(mean(rho_i) - rho_hat))

c("Biased" = rho_hat,
  "De-biased" = rho_hat - bias)


## -----------------------------------------------------------------------------
(se <- sqrt((n-1)*mean((rho_i - mean(rho_i))^2)))
# 95% CI
c("LB" = rho_hat - bias - 1.96*se,
  "UB" = rho_hat - bias + 1.96*se)

