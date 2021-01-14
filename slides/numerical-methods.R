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
# Expected
(0.5 + 0.5) == 1

# Unexpected
(0.1 + 0.2) == 0.3

# Why?
0.3 - (0.1 + 0.2)


## -----------------------------------------------------------------------------
all.equal(0.1 + 0.2, 0.3)
identical(0.1 + 0.2, 0.3)


## -----------------------------------------------------------------------------
# But be careful!
all.equal(1, 2)
# Better
isTRUE(all.equal(1, 2))


## -----------------------------------------------------------------------------
abs(0.3 - (0.2 + 0.1)) < 10^-10


## -----------------------------------------------------------------------------
dpois(100, lambda = 1)
dpois(200, lambda = 1)
# Use logarithms
dpois(200, lambda = 1, log = TRUE)


## -----------------------------------------------------------------------------
n <- 400
# With gamma
(gamma(0.5*(n-1))/(gamma(0.5)*gamma(0.5*(n-2))))
# With lgamma
exp(lgamma(0.5*(n-1)) - lgamma(0.5) - lgamma(0.5*(n-2)))


## ----bisec0, echo=FALSE-------------------------------------------------------
xseq <- seq(-3, 3, length.out = 100)
yseq <- ifelse(xseq < -0.5*pi,
               -0.5, sin(xseq) + 0.5)

# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)


## ----bisec1, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)


## ----bisec2, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)

# Second guess
points(-1.5, sin(-1.5) + 0.5,
       col = "red", pch = 19)
lines(c(-1.5, -1.5), c(0, sin(-1.5) + 0.5),
      lty = 2)


## ----bisec3, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)

# Second guess
points(-1.5, sin(-1.5) + 0.5,
       col = "red", pch = 19)
lines(c(-1.5, -1.5), c(0, sin(-1.5) + 0.5),
      lty = 2)

# Third guess
points(-0.75, sin(-0.75) + 0.5,
       col = "red", pch = 19)
lines(c(-0.75, -0.75), c(0, sin(-0.75) + 0.5),
      lty = 2)


## ----bisec4, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)

# Second guess
points(-1.5, sin(-1.5) + 0.5,
       col = "red", pch = 19)
lines(c(-1.5, -1.5), c(0, sin(-1.5) + 0.5),
      lty = 2)

# Third guess
points(-0.75, sin(-0.75) + 0.5,
       col = "red", pch = 19)
lines(c(-0.75, -0.75), c(0, sin(-0.75) + 0.5),
      lty = 2)

# Fourth guess
points(-0.375, sin(-0.375) + 0.5,
       col = "green", pch = 19)
lines(c(-0.375, -0.375), c(0, sin(-0.375) + 0.5),
      lty = 2)


## ----bisec5, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)

# Second guess
points(-1.5, sin(-1.5) + 0.5,
       col = "red", pch = 19)
lines(c(-1.5, -1.5), c(0, sin(-1.5) + 0.5),
      lty = 2)

# Third guess
points(-0.75, sin(-0.75) + 0.5,
       col = "red", pch = 19)
lines(c(-0.75, -0.75), c(0, sin(-0.75) + 0.5),
      lty = 2)

# Fourth guess
points(-0.375, sin(-0.375) + 0.5,
       col = "green", pch = 19)
lines(c(-0.375, -0.375), c(0, sin(-0.375) + 0.5),
      lty = 2)

# Fifth guess
points(-0.5625, sin(-0.5625) + 0.5,
       col = "red", pch = 19)
lines(c(-0.5625, -0.5625), c(0, sin(-0.5625) + 0.5),
      lty = 2)


## ----bisec6, echo=FALSE-------------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# First guess
points(0, sin(0) + 0.5,
       col = "green", pch = 19)
lines(c(0, 0), c(0, sin(0) + 0.5),
      lty = 2)

# Second guess
points(-1.5, sin(-1.5) + 0.5,
       col = "red", pch = 19)
lines(c(-1.5, -1.5), c(0, sin(-1.5) + 0.5),
      lty = 2)

# Third guess
points(-0.75, sin(-0.75) + 0.5,
       col = "red", pch = 19)
lines(c(-0.75, -0.75), c(0, sin(-0.75) + 0.5),
      lty = 2)

# Fourth guess
points(-0.375, sin(-0.375) + 0.5,
       col = "green", pch = 19)
lines(c(-0.375, -0.375), c(0, sin(-0.375) + 0.5),
      lty = 2)

# Fifth guess
points(-0.5625, sin(-0.5625) + 0.5,
       col = "red", pch = 19)
lines(c(-0.5625, -0.5625), c(0, sin(-0.5625) + 0.5),
      lty = 2)

# Sixth guess
points(-0.46875, sin(-0.46875) + 0.5,
       col = "green", pch = 19)
lines(c(-0.46875, -0.46875), c(0, sin(-0.46875) + 0.5),
      lty = 2)


## -----------------------------------------------------------------------------
a <- 0.5
n <- 20
# First create a function
fun <- function(x) {
  a^2 + x^2 + 2*a*x/(n-1) - n + 2
}


## -----------------------------------------------------------------------------
# Check output at interval bounds
x_lb <- 0 # Lower bound
x_ub <- 5*n # Upper bound

c(fun(x_lb), fun(x_ub))


## -----------------------------------------------------------------------------
# Set up----
x_next <- 0.5*(x_ub - x_lb) + x_lb # Midpoint
epsilon <- 10^-10
f_lb <- fun(x_lb)
f_ub <- fun(x_ub)
f_next <- fun(x_next)
iterations <- 0


## -----------------------------------------------------------------------------
while(abs(f_next) > epsilon) {
  iterations <- iterations + 1
  if (f_ub*f_next > 0) {
    x_ub <- x_next # same sign, move left
    f_ub <- fun(x_ub) } else {
    x_lb <- x_next # opposite sign, move right
    f_lb <- fun(x_lb) }
  x_next <- 0.5*(x_ub - x_lb) + x_lb
  f_next <- fun(x_next)
}


## -----------------------------------------------------------------------------
# Our estimate the solution f(x) = 0
x_next
# Number of iterations
iterations


## -----------------------------------------------------------------------------
# First create a function
g_fun <- function(x) {
  cos(x) - x^3
}


## -----------------------------------------------------------------------------
# Check output at interval bounds
x_lb <- 0 # Lower bound
x_ub <- 2 # Upper bound

c(g_fun(x_lb), g_fun(x_ub))


## -----------------------------------------------------------------------------
# Set up----
x_next <- 0.5*(x_ub - x_lb) + x_lb # Midpoint
epsilon <- 10^-10
g_lb <- g_fun(x_lb)
g_ub <- g_fun(x_ub)
g_next <- g_fun(x_next)
iterations <- 0


## -----------------------------------------------------------------------------
while(abs(g_next) > epsilon) {
  iterations <- iterations + 1
  if (g_ub*g_next > 0) {
    x_ub <- x_next # same sign, move left
    g_ub <- g_fun(x_ub) } else {
    x_lb <- x_next # opposite sign, move right
    g_lb <- g_fun(x_lb) }
  x_next <- 0.5*(x_ub - x_lb) + x_lb
  g_next <- g_fun(x_next)
}


## -----------------------------------------------------------------------------
# Our estimate the solution g(x) = 0
x_next
# Number of iterations
iterations


## -----------------------------------------------------------------------------
# Plot functions to check
xseq <- seq(-2, 2, length.out = 100)
plot(xseq, cos(xseq), type = "l")
lines(xseq, xseq^3)
abline(v = x_next, lty = 2)


## ----brent0, echo = FALSE-----------------------------------------------------
library(PolynomF)
xseq <- seq(-3, 3, length.out = 100)
yseq <- ifelse(xseq < -0.5*pi,
               -0.5, sin(xseq) + 0.5)

# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)


## ----brent1, echo = FALSE-----------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

# Linear interpolation
lines(c(-3, 3), c(-0.5, sin(3) + 0.5),
      lty = 3)
x1 <- -3*sin(3)/(sin(3) + 1)
y1 <- sin(x1) + 0.5

points(x1, y1, pch = 19, col = "green")
lines(c(x1, x1), c(0, y1),
      lty = 2)


## ----brent2, echo = FALSE-----------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

points(x1, y1, pch = 19, col = "green")
lines(c(x1, x1), c(0, y1),
      lty = 2)

# Inverse Quadratic Interpolation I
p <- poly_calc(c(-0.5, sin(3) + 0.5, y1),
               c(-3, 3, x1))

yvec_iqi <- seq(-1, 2, length.out = 100)
xvec_iqi <- p(yvec_iqi)

lines(xvec_iqi, yvec_iqi, lty = 3)

x2 <- p(0)
y2 <- sin(x2) + 0.5

points(x2, y2, pch = 19, col = "red")
lines(c(x2, x2), c(0, y2),
      lty = 2)


## ----brent3, echo = FALSE-----------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

points(x1, y1, pch = 19, col = "green")
lines(c(x1, x1), c(0, y1),
      lty = 2)

points(x2, y2, pch = 19, col = "red")
lines(c(x2, x2), c(0, y2),
      lty = 2)

# Inverse Quadratic Interpolation II
p <- poly_calc(c(-0.5, y1, y2),
               c(-3, x1, x2))

yvec_iqi <- seq(-1, 2, length.out = 100)
xvec_iqi <- p(yvec_iqi)

lines(xvec_iqi, yvec_iqi, lty = 3)

x3 <- p(0)
y3 <- sin(x3) + 0.5

points(x3, y3, pch = 4, col = "black",
       cex = 1.5)
lines(c(x3, x3), c(0, y3),
      lty = 2)


## ----brent4, echo = FALSE-----------------------------------------------------
# Initial setup
plot(xseq, yseq, type = "l",
     ylim = c(-1, 2), xlab = "x", ylab = "f(x)")
abline(h = 0)
points(c(-3, 3), c(-0.5, sin(3) + 0.5),
       col = c("red", "green"), pch = 19)

points(x1, y1, pch = 19, col = "green")
lines(c(x1, x1), c(0, y1),
      lty = 2)

points(x2, y2, pch = 19, col = "red")
lines(c(x2, x2), c(0, y2),
      lty = 2)

# Replace with bisection
x3 <- 0.5*(x2-x1) + x1
y3 <- sin(x3) + 0.5

points(x3, y3, pch = 19, col = "red")
lines(c(x3, x3), c(0, y3),
      lty = 2)


## -----------------------------------------------------------------------------
a <- 0.5
n <- 20
# Create a function
fun <- function(x) {
  a^2 + x^2 + 2*a*x/(n-1) - n + 2
}


## -----------------------------------------------------------------------------
output <- uniroot(f = fun, 
                  interval = c(0, 5*n),
                  tol = 10^-10)
names(output)


## -----------------------------------------------------------------------------
output$root
output$iter


## -----------------------------------------------------------------------------
result <- uniroot(function(x) {
  exp(-x)*(3.2*sin(x) - 0.5*cos(x))
  }, interval = c(3, 4))

result$root

