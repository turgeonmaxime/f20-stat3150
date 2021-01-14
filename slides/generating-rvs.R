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
runif(5)


## -----------------------------------------------------------------------------
lambda <- 1
# We want 1000 samples
n <- 1000
unif_vars <- runif(1000)
exp_vars <- -log(1 - unif_vars)/lambda


## -----------------------------------------------------------------------------
# Compute theoretical quantiles
# using qexp
exp_theo <- qexp(ppoints(n))
qqplot(exp_theo, exp_vars)
# Add diagonal line
abline(a = 0, b = 1)


## ----echo = FALSE, fig.cap="From Wikipedia"-----------------------------------
library(cowplot)
fig_svg <- cowplot::ggdraw() + cowplot::draw_image("../../static/images/Generalized_inversion_method.svg")
plot(fig_svg)


## ----echo = FALSE, fig.width=6, fig.height=6, fig.align="center"--------------
plot(x = c(0, 1), y = c(0, 1), type = "n",
     xaxt = "n", xlab = "x", ylab = expression(F(x)),
     asp = 1, main = expression(p == 0.6))
axis(side = 1, at = c(0L, 1L))
segments(-1, 0, 0)
segments(0, 0.4, 1)
segments(1, 1, 2)
points(x = c(0, 1), y = c(0, 0.4), pch = 21, bg = "white")
points(x = c(0, 1), y = c(0.4, 1), pch = 19)


## -----------------------------------------------------------------------------
p <- 0.6
n <- 1000
unif_vars <- runif(1000)
# as.numeric turns FALSE into 0 
# and TRUE into 1
bern_vars <- as.numeric(unif_vars > 1 - p)


## -----------------------------------------------------------------------------
c(mean(bern_vars), var(bern_vars))
# Compare with theory
c(p, p*(1 - p))


## -----------------------------------------------------------------------------
# Choose degrees of freedom
p <- 2
q <- 4

# rnorm samples from a normal distribution
U <- sum(rnorm(p)^2)
V <- sum(rnorm(q)^2)

# Take ratio
(U/p)/(V/q)


## -----------------------------------------------------------------------------
# What if we want 1000 replicates?
# Use the function replicate!
# First argument: number of replicates
# Second argument: expression to be run multiple times
f_vars <- replicate(1000, {
  U <- sum(rnorm(p)^2)
  V <- sum(rnorm(q)^2)
  (U/p)/(V/q)
})


## -----------------------------------------------------------------------------
qqplot(f_vars, qf(ppoints(1000), p, q))
# Add diagonal line
abline(a = 0, b = 1)


## -----------------------------------------------------------------------------
height_vars <- replicate(1000, {
  sex <- sample(c("M", "F"), size = 1,
                prob = c(0.5, 0.5))
  if (sex == "M") {
    height <- rnorm(1, mean = 175, sd = 7)
  }
  if (sex == "F") {
    height <- rnorm(1, mean = 160, sd = 7)
  }
  return(height)
})


## -----------------------------------------------------------------------------
plot(density(height_vars), ylim = c(0, 0.06))
# Add component densities
x_seq <- seq(140, 200, length.out = 100)
lines(x_seq, dnorm(x_seq, 175, 7), col = "red")
lines(x_seq, dnorm(x_seq, 160, 7), col = "green")


## -----------------------------------------------------------------------------
# Sample with less code
sex <- sample(c(1, 2), size = 1000,
              replace = TRUE, 
              prob = c(0.5, 0.5))
height_vars <- rnorm(1000, c(175, 160)[sex], 7)
c(mean(height_vars), sd(height_vars))


## ---- echo = FALSE------------------------------------------------------------
x_vec <- ppoints(100)
C <- 6
plot(x_vec, dbeta(x_vec, 2, 2), type = "l", ylim = c(0, C),
     xlab = "x", ylab = "Density")
lines(x_vec, C * dunif(x_vec))
polygon(c(0, x_vec, 1, 0), 
        c(0, dbeta(x_vec, 2, 2), 0, 0),
        col = "darkolivegreen3")
polygon(c(0, x_vec, 1, 1, 0, 0), 
        c(0, dbeta(x_vec, 2, 2), 0, C, C, 0),
        col = "indianred1")
arrows(0.78, 0, 0.78, C, code = 3)
text(c(0.5, 0.5), c(0.25, 3), labels = c("Accept", "Reject"), adj = 0.5)


## -----------------------------------------------------------------------------
# Set parameters----
C <- 6 # Constant
n <- 1000 # Number of variates
k <- 0 # counter for accepted
j <- 0 # iterations
y <- numeric(n) # Allocate memory


## -----------------------------------------------------------------------------
# A while loop runs until condition no longer holds
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1) # random variate from g
  if (u < 6*x*(1-x)/C) {
    k <- k + 1
    y[k] <- x
    }
}


## -----------------------------------------------------------------------------
# How many iterations did we need?
j


## -----------------------------------------------------------------------------
# Compare theoretical and empirical quantiles
p <- seq(0.1, 0.9, by = 0.1)
Qhat <- quantile(y, p) # empirical
Q <- qbeta(p, 2, 2) # theoretical


## -----------------------------------------------------------------------------
round(cbind(Qhat, Q, diff = abs(Qhat - Q)), 3)


## ---- echo = FALSE------------------------------------------------------------
x_vec <- ppoints(100)
C <- 1.5
plot(x_vec, dbeta(x_vec, 2, 2), type = "l", ylim = c(0, C),
     xlab = "x", ylab = "Density")
lines(x_vec, C * dunif(x_vec))
polygon(c(0, x_vec, 1, 0), 
        c(0, dbeta(x_vec, 2, 2), 0, 0),
        col = "darkolivegreen3")
polygon(c(0, x_vec, 1, 1, 0, 0), 
        c(0, dbeta(x_vec, 2, 2), 0, C, C, 0),
        col = "indianred1")
arrows(0.78, 0, 0.78, C, code = 3)
text(c(0.5, 0.1), c(0.25, 1.0), labels = c("Accept", "Reject"), adj = 0.5)


## -----------------------------------------------------------------------------
C <- 1.5; k <- j <- 0 # Reset counters
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1)
  if (u < 6*x*(1-x)/C) {
    k <- k + 1
    y[k] <- x
    }
}


## -----------------------------------------------------------------------------
# How many iterations did we need this time?
j

