library(mvtnorm)
library(xgboost)


simulate_dat_10x <- function(n = 1e3) {
  diag_matrix <- diag(rep(3, 10))
  mirrored_matrix <- apply(diag_matrix / 5, 2, rev)
  cov_matrix <- diag_matrix + mirrored_matrix

  x <- rmvnorm(n, rep(1, 10), cov_matrix)
  colnames(x) <- paste0("x", seq_len(ncol(x)))
  y <- 3 * sin(x[, 1]) +
    2.5 * cos(0.3 * x[, 2]) +
    1.12 * x[, 3] +
    sin(x[, 4] * x[, 5]) +
    0.7 * x[, 6] * x[, 7] +
    0.7 * x[, 8] + 0.1 * x[, 9] * x[, 10] + rnorm(n, 0, 0.1)

  list(x = x, y = y)
}

simulate_dat_7x <- function(n = 1e3) {
  diag_matrix <- diag(rep(3, 7))
  mirrored_matrix <- apply(diag_matrix / 5, 2, rev)
  cov_matrix <- diag_matrix + mirrored_matrix

  x <- rmvnorm(n, rep(1, 7), cov_matrix)
  colnames(x) <- paste0("x", seq_len(ncol(x)))
  y <- 3 * sin(x[, 1]) +
    2.5 * cos(0.3 * x[, 2]) +
    1.12 * x[, 3] +
    sin(x[, 4] * x[, 5]) +
    0.7 * x[, 6] * x[, 7] + rnorm(n, 0, 0.1)

  list(x = x, y = y)
}

simulate_dat_wrapped <- function(n, c, s = FALSE) {
  if (s) {
    mu <- rep(0, 100)
    sigma <- diag(nrow = 100)
    sigma[1, 2] <- c
    sigma[2, 1] <- c
  } else {
    mu <- rep(0, 2)
    sigma <- toeplitz(c^(0:1))
  }

  list(dat = simulate_dat(mu, sigma, n), cov_args = list(mu, sigma))
}

true_shap <- function(x1, x2, coord = 1, cov_base = 0.3) {
  if (coord == 1) {
    x1 + x1 * x2 - cov_base
  } else {
    x2 + x1 * x2 - cov_base
  }
}

true_shap2 <- function(x, coord = 1, cov_base = 0.3) {
  x[, ..coord] + x[, 1] * x[, 2] - cov_base
}

true_components_m <- function(x, cov_base = 0.3) {
  data.table(
    x1 = x[, 1] - 2 * cov_base,
    x2 = x[, 2] - 2 * cov_base,
    "x1:x2" = 2 * x[, 1] * x[, 2] + 2 * cov_base
  )
}

probFunction_ <- function(coords, lb, ub, cov_args) {
  pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

probFunctionEmp_ <- function(coords, lb, ub, x) {
  mean(apply(t(x[, coords]) > lb & t(x[, coords]) < ub, 2, all))
}

cov_args <- list(c(0, 0), toeplitz(0.3^(0:1)))
probFunction <- function(...) probFunction_(..., cov_args)

probFunctionEmp <- function(...) probFunctionEmp_(..., dataset$x)
probFunctionEmp2 <- function(...) probFunctionEmp_(..., dataset$x_og)
