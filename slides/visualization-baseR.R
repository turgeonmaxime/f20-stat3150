## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)


## ----eval = FALSE-------------------------------------------------------------
## # Switch to litres per 100km
## transform(mtcars, litres_per_100km = mpg/235.215)


## ----eval = FALSE-------------------------------------------------------------
## # Only keep rows where cyl is equal to 6 or 8
## subset(mtcars, subset = cyl %in% c(6, 8))
## # Only keep cyl and mpg columns
## subset(mtcars, select = c(cyl, mpg))


## ----eval = TRUE, message = FALSE---------------------------------------------
# Average mpg for each value of cyl
tapply(mtcars$mpg, mtcars$cyl, mean)


## ---- message=FALSE-----------------------------------------------------------
library(dslabs)

# Create histogram for oleic acid
hist(olive$oleic)


## ----hist_fill, message=FALSE, fig.keep="last"--------------------------------
# Look at distribution by region
# using stacked histograms
top_hist <- hist(olive$oleic)
# Gray: North; Red: Sardinia; Blue: South
hist(olive$oleic[olive$region != "Northern Italy"],
     breaks = top_hist$breaks,
     col = "red", add = TRUE)
hist(olive$oleic[olive$region == "Southern Italy"],
     breaks = top_hist$breaks,
     col = "blue", add = TRUE)


## ----hist_facet, message=FALSE, eval = TRUE-----------------------------------
# Split plotting device in three
par(mfrow = c(1,3))
hist(olive$oleic[olive$region == "Northern Italy"])
hist(olive$oleic[olive$region == "Sardinia"])
hist(olive$oleic[olive$region == "Southern Italy"])


## ----eval = FALSE-------------------------------------------------------------
## # Instead of this:
## hist(olive$oleic)
## # You can write this:
## with(olive, hist(oleic))


## ----hist_facet2, message=FALSE, eval = TRUE----------------------------------
# Same as above, but using with
par(mfrow = c(1,3))
# 2nd argument of with is an expression 
# i.e. everything between {}
with(olive, {
  hist(oleic[region == "Northern Italy"])
  hist(oleic[region == "Sardinia"])
  hist(oleic[region == "Southern Italy"])
})


## ---- message=FALSE, eval = FALSE---------------------------------------------
## # A more complex example
## par(mfrow = c(1,3))
## hist_bg <- hist(olive$oleic, col = 'white',
##                 main = "Northern Italy")
## with(olive, {
##   hist(oleic[region == "Northern Italy"],
##        col = 2, add = TRUE, breaks = hist_bg$breaks)
##   plot(hist_bg, main = "Sardinia")
##   hist(oleic[region == "Sardinia"],
##        col = 3, add = TRUE, breaks = hist_bg$breaks)
##   plot(hist_bg, main = "Southern Italy")
##   hist(oleic[region == "Southern Italy"],
##        col = 4, add = TRUE, breaks = hist_bg$breaks)
## })


## ----hist_complex, message=FALSE, eval = TRUE, echo = FALSE-------------------
# A more complex example
par(mfrow = c(1,3))
hist_bg <- hist(olive$oleic, col = 'white', 
                main = "Northern Italy")
with(olive, {
  hist(oleic[region == "Northern Italy"],
       col = 2, add = TRUE, breaks = hist_bg$breaks)
  plot(hist_bg, main = "Sardinia")
  hist(oleic[region == "Sardinia"],
       col = 3, add = TRUE, breaks = hist_bg$breaks)
  plot(hist_bg, main = "Southern Italy")
  hist(oleic[region == "Southern Italy"],
       col = 4, add = TRUE, breaks = hist_bg$breaks)
})


## ---- message=FALSE-----------------------------------------------------------
plot(density(olive$oleic))

# By region--first try
with(olive, {
  plot(density(oleic[region == "Northern Italy"]))
  lines(density(oleic[region == "Sardinia"]))
  lines(density(oleic[region == "Southern Italy"]))
})


## ---- message=FALSE-----------------------------------------------------------
# By region--second try
dens_ni <- with(olive, 
           density(oleic[region == "Northern Italy"]))
dens_sa <- with(olive, 
           density(oleic[region == "Sardinia"]))
dens_si <- with(olive, 
           density(oleic[region == "Southern Italy"]))

str(dens_ni)


## -----------------------------------------------------------------------------
xlim <- range(c(dens_ni$x, dens_sa$x, dens_si$x))
ylim <- range(c(dens_ni$y, dens_sa$y, dens_si$y))

plot(dens_ni, xlim = xlim, ylim = ylim, col = 2)
lines(dens_sa, lty = 2, col = 3)
lines(dens_si, lty = 3, col = 4)


## ---- message=FALSE-----------------------------------------------------------
plot(ecdf(olive$oleic))

# By region--first try
with(olive, {
  plot(ecdf(oleic[region == "Northern Italy"]))
  lines(ecdf(oleic[region == "Sardinia"]))
  lines(ecdf(oleic[region == "Southern Italy"]))
})


## ---- message=FALSE-----------------------------------------------------------
# By region--second try
ecdf_ni <- with(olive, 
                ecdf(oleic[region == "Northern Italy"]))
ecdf_sa <- with(olive,
                ecdf(oleic[region == "Sardinia"]))
ecdf_si <- with(olive, 
                ecdf(oleic[region == "Southern Italy"]))

str(ecdf_ni) # This is a function!


## -----------------------------------------------------------------------------
xlim <- range(olive$oleic)

plot(ecdf_ni, xlim = xlim, col = 2)
lines(ecdf_sa, col = 3)
lines(ecdf_si, col = 4)


## ---- message=FALSE-----------------------------------------------------------
boxplot(olive$oleic)

# Split by region using a formula
boxplot(oleic ~ region, data = olive)

# Flip boxplots
boxplot(oleic ~ region, data = olive,
        horizontal = TRUE)


## -----------------------------------------------------------------------------
plot(stars$magnitude,
     stars$temp)

# Add colour for type of stars
with(stars, plot(magnitude, temp,
                 col = factor(type)))


## ---- echo = TRUE, eval = TRUE------------------------------------------------
library(scatterplot3d)
library(tidyr)
wide_data <- spread(greenhouse_gases,
                    gas, concentration)

head(wide_data, n = 3)

with(wide_data,
     scatterplot3d(CH4,   # x axis
                   CO2,   # y axis
                   N2O    # z axis
))


## -----------------------------------------------------------------------------
library(MASS)

image(kde2d(stars$magnitude,
            stars$temp))


## ---- message = FALSE---------------------------------------------------------
# Select three variables
olive_sub <- subset(olive, 
                    select = c(eicosenoic, arachidic, 
                               linolenic))

plot(olive_sub)


## ---- eval = FALSE------------------------------------------------------------
## # Or alternatively:
## pairs(olive_sub)


## -----------------------------------------------------------------------------
# How to put histograms on the diagonal?
## From the help page for graphics::pairs
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan",...)
}


## -----------------------------------------------------------------------------
pairs(olive_sub,
      diag.panel = panel.hist)

