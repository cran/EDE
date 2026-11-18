## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5)
sim_file <- system.file("extdata", "simulation_summary.csv", package = "EDE")
have_sim <- nzchar(sim_file)

## ----echo = FALSE, results = "asis", eval = have_sim--------------------------
cat("The results below were produced by `data-raw/simulation.R` in the package source.\n")

## ----eval = have_sim, echo = FALSE--------------------------------------------
sim <- read.csv(sim_file)
sc_lab <- c(uniform = "uniform", beta2 = "beta2", beta4 = "beta4",
            eff_dec = "eff_dec", eff_inc = "eff_inc")
wide <- function(v, n) {
  d <- sim[sim$n == n, c("method", "scenario", v)]
  w <- reshape(d, idvar = "method", timevar = "scenario", direction = "wide")
  names(w) <- sub(paste0("^", v, "\\."), "", names(w))
  w <- w[, c("method", names(sc_lab))]
  rownames(w) <- NULL
  w
}

## ----eval = !have_sim, echo = FALSE, results = "asis"-------------------------
# cat("*The simulation results are not installed in this build.*\n")

## ----eval = have_sim, echo = FALSE--------------------------------------------
cv <- wide("coverage_all", 20)
knitr::kable(cv[!is.na(rowSums(cv[, -1])), ], digits = 2, row.names = FALSE)

## ----eval = have_sim, echo = FALSE, fig.height = 6.5--------------------------
meths <- c("solow1993", "solow1993b", "burgman1995", "mcinerny2006",
           "strauss1989", "robson1964", "hall_wang1999", "ole")
cols <- c("#1b6ca8", "#c05621", "#2f855a", "#805ad5",
          "#b7791f", "#2d3748", "#d53f8c", "#718096")
op <- par(mfrow = c(2, 3), mar = c(4, 4, 2, 0.5))
for (sc in names(sc_lab)) {
  plot(NA, xlim = c(10, 50), ylim = c(0, 1), xlab = "sighting events",
       ylab = "coverage", main = sc, log = "x", xaxt = "n")
  axis(1, c(10, 20, 50))
  abline(h = 0.95, lty = 2, col = "grey50")
  for (i in seq_along(meths)) {
    d <- sim[sim$scenario == sc & sim$method == meths[i], ]
    lines(d$n, d$coverage_all, type = "b", col = cols[i], pch = 16, lwd = 1.5)
  }
}
plot.new()
legend("center", legend = meths, col = cols, pch = 16, lwd = 1.5, bty = "n")
par(op)

## ----eval = have_sim, echo = FALSE--------------------------------------------
key <- c("hall_wang1999", "solow1993b", "robson1964", "strauss1989", "solow1993", "ole")
fin <- wide("share_finite", 20); exc <- wide("excess_upper", 20)
fin <- fin[fin$method %in% key, ]; exc <- exc[exc$method %in% key, ]
out <- data.frame(method = fin$method,
                  setNames(lapply(names(sc_lab), function(s)
                    sprintf("%.0f%% / %+.0f", 100 * fin[[s]], exc[[s]])), names(sc_lab)))
knitr::kable(out, row.names = FALSE, align = "lrrrrr",
             col.names = c("method", names(sc_lab)))

## ----eval = have_sim, echo = FALSE--------------------------------------------
pb <- wide("bias_median", 20)
pb <- pb[!is.na(rowSums(pb[, -1])), ]
knitr::kable(pb, digits = 1, row.names = FALSE)

