rm(list = ls())
# Example: n1=509 test cells
# background rate 0,0067 translocations/cell
# type I error alpha = 0,05
# type II error beta = 0,1

mu0 <- 0.0067
# default 1 / 1000 for dicentrics
# for translocations transformed via Sigurnd
alpha <- 0.05 # type 1 error rate
beta <- 0.1 # type 2 error rate
n1 <- 1000

# Calculation of the decision threshold
# constant background rate mu0

y <- 1:100
y_d_const <- min(y[which(sapply(y, function(x) stats::ppois(x, mu0 * n1, lower.tail = FALSE)) <= alpha / 2)])
cat("decision threshold:", y_d_const)

y_z_const <- stats::qchisq(1 - beta, 2 * (y_d_const + 1)) / 2
cat("detection limit:", y_z_const)

# Additionally we can project the y into dose using the curve
