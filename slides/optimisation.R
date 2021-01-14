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
# Create functions
gradient <- function(x) -2*x + cos(x)
hessian <- function(x) -2 - sin(x)
# Set-up parameters
x_current <- 1
x_next <- x_current - 
  gradient(x_current)/hessian(x_current)
tol <- 10^-10
iter <- 1


## -----------------------------------------------------------------------------
while (abs(x_current - x_next) > tol &
       iter < 100) {
  iter <- iter + 1
    x_current <- x_next
    step <- gradient(x_current)/hessian(x_current)
    x_next <- x_current - step
}
x_next


## -----------------------------------------------------------------------------
plot(function(x) -x^2 + sin(x), from = -1, to = 1)
abline(v = x_next, lty = 2)


## -----------------------------------------------------------------------------
gradient <- function(x) 5 + 10*x - 5/(1+exp(x))
hessian <- function(x) 10 + 5*exp(x)/(1+exp(x))^2

x_current <- 1
x_next <- x_current -
  gradient(x_current)/hessian(x_current)
tol <- 10^-10
iter <- 1


## -----------------------------------------------------------------------------
while (abs(x_current - x_next) > tol &
       iter < 100) {
  iter <- iter + 1
    x_current <- x_next
    step <- gradient(x_current)/hessian(x_current)
    x_next <- x_current - step
}
x_next


## -----------------------------------------------------------------------------
library(asbio)
data(crabs)
y_vec <- crabs$satell
x_vec <- crabs$weight

gradient <- function(theta) {
  mu_x <- exp(theta[1] + theta[2]*x_vec)
  ell_b0 <- sum(y_vec - mu_x)
  ell_b1 <- sum(y_vec*x_vec - x_vec*mu_x)
  return(c(ell_b0, ell_b1))
}


## -----------------------------------------------------------------------------
hessian <- function(theta) {
  mu_x <- exp(theta[1] + theta[2]*x_vec)
  ell_b0b0 <- -sum(mu_x)
  ell_b1b1 <- -sum(x_vec^2*mu_x)
  ell_b0b1 <- -sum(x_vec*mu_x)
  
  hess <- matrix(c(ell_b0b0, ell_b0b1,
                   ell_b0b1, ell_b1b1),
                 ncol = 2)
  return(hess)
}


## -----------------------------------------------------------------------------
# Set-up variables
x_current <- c(0, 1)
x_next <- x_current - solve(hessian(x_current), 
                            gradient(x_current))
tol <- 10^-10
iter <- 1


## -----------------------------------------------------------------------------
while(sum((x_current - x_next)^2) > tol & 
      iter < 100) {
  iter <- iter + 1
  x_current <- x_next
  step <- solve(hessian(x_current), 
                gradient(x_current))
  x_next <- x_current - step
}
x_next


## -----------------------------------------------------------------------------
fit <- glm(satell ~ weight, data = crabs,
           family = poisson)
coef(fit)


## ---- message = FALSE---------------------------------------------------------
library(tidyverse)
log_lik <- function(row) {
    mu_vec <- exp(row$beta0 + row$beta1*x_vec)
    sum(dpois(y_vec, lambda = mu_vec, log = TRUE))
}

data_plot <- expand.grid(beta0 = seq(-2, 0, 0.1),
                         beta1 = seq(0, 1, 0.1)) %>% 
  purrrlyr::by_row(log_lik, .collate = "cols", 
                   .to = "loglik")


## ---- message = FALSE---------------------------------------------------------
head(data_plot)


## ---- message = FALSE---------------------------------------------------------
data_plot %>% 
  ggplot(aes(x = beta0, y = beta1,
             z = loglik)) + 
  geom_contour() + 
  geom_point(x = coef(fit)[1],
             y = coef(fit)[2],
             shape = 4)


## ----echo = FALSE-------------------------------------------------------------
nr_vec <- matrix(NA, nrow = iter + 1, ncol = 2)
nr_vec[1, ] <- c(0, 1)
for (i in 1:iter) {
  step <- solve(hessian(nr_vec[i,]), 
                gradient(nr_vec[i,]))
  nr_vec[i+1,] <- nr_vec[i,] - step
}


## ---- echo = FALSE------------------------------------------------------------
colnames(nr_vec) <- c("beta0", "beta1")
data_plot %>% 
  ggplot(aes(x = beta0, y = beta1)) + 
  geom_contour(aes(z = loglik)) + 
  geom_point(x = coef(fit)[1],
             y = coef(fit)[2],
             shape = 4) +
  geom_path(data = data.frame(nr_vec)) +
  geom_point(data = data.frame(nr_vec),
             col = "black", size = 0.8)

