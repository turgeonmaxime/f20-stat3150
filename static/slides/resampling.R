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
library(DAAG)
library(ggplot2)

# Fit model
fit1 <- lm(magnetic ~ chemical, data=ironslag)
coef(fit1)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
ggplot(ironslag, aes(chemical, magnetic)) +
  geom_point() + 
  geom_abline(intercept = coef(fit1)[1],
              slope = coef(fit1)[2])


## -----------------------------------------------------------------------------
# Fitted against residuals
plot(fitted(fit1), resid(fit1)) 
abline(h = 0)


## -----------------------------------------------------------------------------
# Histogram of student residuals
hist(rstudent(fit1), 20)


## -----------------------------------------------------------------------------
# Histogram of Cook's distance
hist(cooks.distance(fit1), 20)


## -----------------------------------------------------------------------------
library(MASS)
library(ggplot2)

# Fit model
fit2 <- lm(log(brain) ~ log(body), data = mammals)
coef(fit2)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
ggplot(mammals, aes(log(body), log(brain))) +
  geom_point() + 
  geom_abline(intercept = coef(fit2)[1],
              slope = coef(fit2)[2])


## -----------------------------------------------------------------------------
# Fitted against residuals
plot(fitted(fit2), resid(fit2)) 
abline(h = 0)


## -----------------------------------------------------------------------------
# Histogram of student residuals
hist(rstudent(fit2), 20)


## -----------------------------------------------------------------------------
# Histogram of Cook's distance
hist(cooks.distance(fit2), 20)


## ----boot_cases, cache = TRUE-------------------------------------------------
n <- nrow(ironslag)
boot_beta1 <- replicate(5000, {
  indices <- sample(n, n, replace = TRUE)
  fit_boot <- lm(magnetic ~ chemical, 
                 data = ironslag[indices, ])
  coef(fit_boot)
})


## -----------------------------------------------------------------------------
str(boot_beta1)

se_int <- sd(boot_beta1[1,])
se_slope <- sd(boot_beta1[2,])

cbind("Lower" = coef(fit1) - 1.96*c(se_int, se_slope),
      "Upper" = coef(fit1) + 1.96*c(se_int, se_slope))


## -----------------------------------------------------------------------------
# Compare to MLE theory
confint(fit1)


## ----boot_resids, cache = TRUE------------------------------------------------
# Compute residuals
resids <- resid(fit2)

n <- length(resids)
boot_beta2 <- replicate(5000, {
  indices <- sample(n, n, replace = TRUE)
  logbrain_boot <- fitted(fit2) + resids[indices]
  fit_boot <- lm(logbrain_boot ~ log(mammals$body))
  coef(fit_boot)
})


## -----------------------------------------------------------------------------
str(boot_beta2)

se_int <- sd(boot_beta2[1,])
se_slope <- sd(boot_beta2[2,])

cbind("Lower" = coef(fit2) - 1.96*c(se_int, se_slope),
      "Upper" = coef(fit2) + 1.96*c(se_int, se_slope))


## -----------------------------------------------------------------------------
# Compare to MLE theory
confint(fit2)

