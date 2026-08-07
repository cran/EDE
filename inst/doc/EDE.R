## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 5)

## -----------------------------------------------------------------------------
library(EDE)

years <- c(1900, 1902, 1903, 1905, 1907, 1908, 1910, 1912, 1915, 1918,
           1920, 1923, 1925, 1928, 1930, 1933, 1936)
sightings <- c(4, 3, 5, 2, 3, 4, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1)

sd <- sighting_data(data.frame(year = years, sightings = sightings))
sd

## -----------------------------------------------------------------------------
ole(sd)
robson1964(sd)
strauss1989(sd)
solow1993(sd, test_year = 2000)
solow1993b(sd, test_year = 2000)
solow2005(sd, test_year = 2000)
burgman1995(sd, test_year = 2000)

## -----------------------------------------------------------------------------
curve_1993 <- solow1993(sd, test_year = 2000, data_out = TRUE)
curve_2005 <- solow2005(sd, test_year = 2000, data_out = TRUE)

plot(curve_1993$time, curve_1993$chance, type = "l", lwd = 2, col = "steelblue",
     ylim = c(0, 1), xlab = "candidate extinction year", ylab = "chance of persistence")
lines(curve_2005$time, curve_2005$chance, lwd = 2, col = "firebrick")
abline(h = 0.05, lty = 2, col = "grey50")
legend("topright", legend = c("Solow (1993)", "Solow (2005)", "alpha = 0.05"),
       col = c("steelblue", "firebrick", "grey50"), lty = c(1, 1, 2), lwd = c(2, 2, 1), bty = "n")

