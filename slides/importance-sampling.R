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
# Sample size
n <- 5000
# Define a function for integrand
integrand <- function(x) {
  # We want to multiply by zero if outside the range
  supp_ind <- as.numeric(x > 0 & x < 1)
  
  return(supp_ind*exp(-x)/(1 + x^2))
}


## -----------------------------------------------------------------------------
# Look at the graph of the function
xvar <- seq(-0.5, 1.5, by = 0.01)
plot(xvar, integrand(xvar), type = "l")


## -----------------------------------------------------------------------------
# 1. Basic MC integration
unif_vars <- runif(n)

theta1 <- mean(integrand(unif_vars))
sd1 <- sd(integrand(unif_vars))


## -----------------------------------------------------------------------------
# 2. Exponential density
exp_vars <- rexp(n)

theta2 <- mean(integrand(exp_vars)/dexp(exp_vars))
sd2 <- sd(integrand(exp_vars)/dexp(exp_vars))


## -----------------------------------------------------------------------------
# Compare results
c(theta1, theta2)
c(sd1, sd2)/sqrt(n)


## -----------------------------------------------------------------------------
# How many are zeros?
sum(integrand(exp_vars) == 0)


## -----------------------------------------------------------------------------
# 3. Truncated exponential density
unif_vars <- runif(n)
truncexp_vars <- -log(1 - unif_vars*(1 - exp(-1)))

# Evaluate the density at those points
phi_vars <- exp(-truncexp_vars)/(1 - exp(-1))

theta3 <- mean(integrand(truncexp_vars)/phi_vars)
sd3 <- sd(integrand(truncexp_vars)/phi_vars)


## -----------------------------------------------------------------------------
# Compare results
c(theta1, theta2, theta3)
c(sd1, sd2, sd3)/sqrt(n)


## -----------------------------------------------------------------------------
# Points between 0 and 1 without boundary
xvar <- ppoints(100)
plot(xvar, integrand(xvar), type = "l")
lines(xvar, integrand(xvar)/exp(-xvar), col = "red")
legend(x = "topright", 
       legend = c("Uniform", "Trunc. Exp."), 
       col = c("black", "red"), lty = 1)


## -----------------------------------------------------------------------------
n <- 5000
norm_vars <- rnorm(n)
# Average of 0s and 1s gives proportion of 1s
mean(norm_vars > 5)


## -----------------------------------------------------------------------------
n <- 10000000
norm_vars <- rnorm(n)
# Average of 0s and 1s gives proportion of 1s
mean(norm_vars > 5)


## -----------------------------------------------------------------------------
# Check if > 5 in absolute value, and divide by 2
0.5*mean(abs(norm_vars) > 5)
# Compare both standard errors
c(sd(norm_vars > 5), 0.5*sd(abs(norm_vars) > 5))


## -----------------------------------------------------------------------------
# Shifted exponential density
unif_vars <- runif(n)
shiftexp_vars <- -log(unif_vars) + 5

# Evaluate the density at those points
phi_vars <- exp(-(shiftexp_vars - 5))

theta_est <- mean(dnorm(shiftexp_vars)/phi_vars)
sd_est <- sd(dnorm(shiftexp_vars)/phi_vars)


## -----------------------------------------------------------------------------
# Compare all three approaches
c("Method1" = mean(norm_vars > 5), 
  "Method2" = 0.5*mean(abs(norm_vars) > 5),
  "Method 3" = theta_est)

c("Method1" = sd(norm_vars > 5), 
  "Method2" = 0.5*sd(abs(norm_vars) > 5),
  "Method 3" = sd_est)


## -----------------------------------------------------------------------------
pi_seq <- ppoints(100)
posterior <- 1*choose(294, 32)*pi_seq^32*(1 - pi_seq)^262

plot(pi_seq, posterior, type = "l")


## ---- eval = FALSE, echo = FALSE----------------------------------------------
## # Let's look at the ratio
## plot(pi_seq, posterior/dbeta(pi_seq, 2, 10),
##      type = "l")


## -----------------------------------------------------------------------------
# Create a function for posterior and weight
post_fun <- function(pi) {
  1*choose(294, 32)*pi^32*(1 - pi)^262
}

weight <- function(pi) {
  post_fun(pi)/dbeta(pi, shape1 = 2, 
                     shape2 = 10)
}


## -----------------------------------------------------------------------------
# Assume we are interested in posterior mean
# so g(x) = x
n <- 5000
beta_vars <- rbeta(n, shape1 = 2, shape2 = 10)

denominator <- weight(beta_vars)
numerator <- weight(beta_vars)*beta_vars
(theta <- mean(numerator)/mean(denominator))


## -----------------------------------------------------------------------------
# What about posterior variance?
denominator <- weight(beta_vars)
numerator <- weight(beta_vars)*beta_vars^2 # g(x) = x^2
theta2 <- mean(numerator)/mean(denominator)
# Var(X) = E(X^2) - E(X)^2
theta2 - theta^2


## -----------------------------------------------------------------------------
# What about posterior probability that pi
# is between 0.08 and 0.12?
denominator <- weight(beta_vars)
# g(x) is indicator function
gvals <- as.numeric(beta_vars > 0.08 &
                      beta_vars < 0.12)
numerator <- weight(beta_vars)*gvals
mean(numerator)/mean(denominator)

