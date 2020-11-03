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
n <- 1000
unif_vars <- runif(n)
mean(exp(-unif_vars))
# Compare to actual value
1 - exp(-1)


## -----------------------------------------------------------------------------
unif_vars <- runif(n, max = 2)
mean(exp(-unif_vars))
# Compare to actual value
1 - exp(-2)


## -----------------------------------------------------------------------------
2*mean(exp(-unif_vars))


## -----------------------------------------------------------------------------
n <- 1000
exp_vars <- rexp(n)
mean(1/(1 + exp_vars))


## -----------------------------------------------------------------------------
# The first uniform example
n <- 1000
unif_vars <- runif(n)
theta_hat <- mean(exp(-unif_vars))
sigma_hat <- sd(exp(-unif_vars))

c("Lower" = theta_hat - 1.96*sigma_hat/sqrt(n),
  "Upper" = theta_hat + 1.96*sigma_hat/sqrt(n))


## -----------------------------------------------------------------------------
# Exponential example
exp_vars <- rexp(n)
theta_hat <- mean(1/(1 + exp_vars))
sigma_hat <- sd(1/(1 + exp_vars))

c("Lower" = theta_hat - 1.96*sigma_hat/sqrt(n),
  "Upper" = theta_hat + 1.96*sigma_hat/sqrt(n))


## -----------------------------------------------------------------------------
library(dplyr)
# Recall our first example
n <- 1000
unif_vars <- runif(n)
theta_hat <- cummean(exp(-unif_vars))

plot(theta_hat,
     type = "l")


## -----------------------------------------------------------------------------
sigma2_hat <- cumstats::cumvar(exp(-unif_vars))
sigma_hat <- sqrt(sigma2_hat)

plot(theta_hat, type = "l")
lines(theta_hat + 1.96*sigma_hat/sqrt(seq(1, n)), 
      lty = 2)
lines(theta_hat - 1.96*sigma_hat/sqrt(seq(1, n)), 
      lty = 2)


## -----------------------------------------------------------------------------
n <- 1000
cauchy_vars <- rcauchy(n)
theta_hat <- cummean(cauchy_vars)

plot(theta_hat,
     type = "l")


## -----------------------------------------------------------------------------
n <- 1000000
unif_vars <- runif(n)
theta_hat <- mean(1/unif_vars)
sigma_hat <- sd(1/unif_vars)
c(theta_hat, sigma_hat/sqrt(n))


## -----------------------------------------------------------------------------
# Let's look at a trace plot
theta_hat <- cummean(1/unif_vars)
# We'll only look at every 100th value
index_val <- seq(100, n, by = 100)
plot(x = index_val,
     y = theta_hat[index_val],
     type = "l")


## -----------------------------------------------------------------------------
# Going back to second example
# Recall: Need to multiply by 2!
n <- 1000
unif_vars <- runif(n, max = 2)
theta_hat <- 2*mean(exp(-unif_vars))
sigma_hat <- 2*sd(exp(-unif_vars))
sigma_hat/sqrt(n)


## ----echo = FALSE-------------------------------------------------------------
options(scipen = 999)


## -----------------------------------------------------------------------------
# What if we want a standard error of 0.0001?
factor <- (sigma_hat/sqrt(n)/0.0001)^2
(n2 <- factor * n)

unif_vars2 <- runif(n2, max = 2)
2*sd(exp(-unif_vars2))/sqrt(n2)


## -----------------------------------------------------------------------------
# Classical approach
n <- 1000
unif_vars <- runif(n)
theta_hat <- mean(sin(0.5*pi*unif_vars))
sigma_hat <- sd(sin(0.5*pi*unif_vars))
c(theta_hat, sigma_hat/sqrt(n))


## -----------------------------------------------------------------------------
# Antithetic variables
n <- 500
unif_vars <- runif(n)
theta_hat <- mean(sin(0.5*pi*c(unif_vars,
                               1 - unif_vars)))
sigma_hat <- sd(sin(0.5*pi*c(unif_vars,
                               1 - unif_vars)))
c(theta_hat, sigma_hat/sqrt(2*n))


## -----------------------------------------------------------------------------
# Classical approach
n <- 1000
exp_vars <- rexp(n)
theta_hat <- mean(1/(1 + exp_vars))
sigma_hat <- sd(1/(1 + exp_vars))
c(theta_hat, sigma_hat/sqrt(n))


## -----------------------------------------------------------------------------
# Antithetic variables
n <- 1000
unif_vars <- runif(n)
exp_vars <- c(-log(unif_vars), -log(1 - unif_vars))
theta2_hat <- mean(1/(1 + exp_vars))
sigma2_hat <- sd(1/(1 + exp_vars))
c(theta2_hat, sigma2_hat/sqrt(2*n))


## -----------------------------------------------------------------------------
n <- 1000
exp_vars <- rexp(n)
g_est <- 1/(1 + exp_vars)
h_est <- 1 + exp_vars

(c_star <- -cov(g_est, h_est)) # Var(h(X)) = 1
thetac_hat <- mean(g_est + c_star*(h_est - 2))
sigmac_hat <- sd(g_est + c_star*(h_est - 2))
c(thetac_hat, sigmac_hat/sqrt(n))


## -----------------------------------------------------------------------------
# Compare variance of classical MC vs control vars
(var(g_est) - sigmac_hat^2) / var(g_est)

