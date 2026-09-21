## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 5)

## -----------------------------------------------------------------------------
library(EDE)

years <- c(1900, 1902, 1903, 1905, 1907, 1908, 1910, 1912,
           1915, 1918, 1920, 1923, 1925, 1928, 1930, 1933, 1936)
counts <- c(4, 3, 5, 2, 3, 4, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1)
sd <- sighting_data(data.frame(year = years, sightings = counts))

## -----------------------------------------------------------------------------
robson1964(sd)
strauss1989(sd)
ole(sd)
ole(sd, k = 10)

## -----------------------------------------------------------------------------
solow1993(sd, test_year = 2000)
solow1993b(sd, test_year = 2000)
solow2005(sd, test_year = 2000)
mcinerny2006(sd, test_year = 2000)
burgman1995(sd, test_year = 2000)
solow_roberts2003(sd, test_year = 2000)
jaric2010(sd, test_year = 2000)

## -----------------------------------------------------------------------------
curve <- jaric2010(sd, test_year = 2000, data_out = TRUE)
plot(curve$time, curve$chance, type = "l",
     xlab = "candidate time", ylab = "p-value")
abline(h = 0.05, lty = 2)

## -----------------------------------------------------------------------------
uncertain <- sighting_data(data.frame(
  year = c(1900, 1910, 1920, 1930),
  sightings = 1
))

jaric_roberts2014(
  uncertain,
  reliability = c(1.0, 0.9, 0.6, 0.3)
)

