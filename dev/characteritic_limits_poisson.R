# rm(list = ls())
# Example: n1=509 test cells
# background rate 0,0067 translocations/cell
# type I error alpha = 0,05
# type II error beta = 0,1

mu0 <- 0.0067
n1 <- 509
alpha <- 0.05 # type 1 error rate
beta <- 0.1 # type 2 error rate

# Calculation of the decision threshold
# constant background rate mu0

y <- 1:100
y_d_const <- min(y[which(sapply(y, function(x) ppois(x, mu0 * n1, lower.tail = FALSE)) <= alpha / 2)])
cat("decision threshold:", y_d_const)
# a partir de 7 detecta radiació i per sota és background noise

# Calculation of the decision threshold constant variable mu0
# background cells
# n0=20000

fun.dec.thresh.variable <- function(y, y0, n1, n0) {
  y.seq <- 0:y
  p <- n1 / (n1 + n0)
  return(1 - sum(exp(lchoose(y0 + y + 1, y.seq) + y.seq * log(p) + (y0 + y + 1 - y.seq) * log(1 - p))))
}

n0 <- 2e4
y_d_var <- min(y[which(sapply(y, fun.dec.thresh.variable,
  n1 = n1, n0 = n0, y0 = mu0 * n0
) <= alpha / 2)])
cat("decision threshold:", y_d_var)


# P value for the comparison of two poison rates
cat(
  "P(y_d_var vs background)=",
  poisson.test(c(y_d_var, mu0 * n0), c(n1, n0))$p.value
)
cat(
  "P(y_d_var + 1 vs background)=",
  poisson.test(c(y_d_var + 1, mu0 * n0), c(n1, n0))$p.value
)
# calculation of the detection limit:
y_z_const <- qchisq(1 - beta, 2 * (y_d_const + 1)) / 2
cat("detection limit:", y_z_const)
y_z_var <- qchisq(1 - beta, 2 * (y_d_var + 1)) / 2
cat("detection limit:", y_z_var)
