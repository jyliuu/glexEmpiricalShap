library(mvtnorm)

compute_lb_ub <- function(tree, n_features) {
  max_node <- tree[, max(Node)]
  num_nodes <- max_node + 1
  lb <- matrix(-Inf, nrow = num_nodes, ncol = n_features)
  ub <- matrix(Inf, nrow = num_nodes, ncol = n_features)
  for (nn in 0:max_node) {
    if (tree[Node == nn, !is.na(Yes)]) {
      left_child <- tree[Node == nn, Yes]
      right_child <- tree[Node == nn, No]
      splitvar <- tree[Node == nn, Feature_num]

      # Children inherit bounds
      ub[left_child + 1, ] <- ub[nn + 1, ]
      ub[right_child + 1, ] <- ub[nn + 1, ]
      lb[left_child + 1, ] <- lb[nn + 1, ]
      lb[right_child + 1, ] <- lb[nn + 1, ]

      # Restrict by new split
      ub[left_child + 1, splitvar] <- tree[Node == nn, Split]
      lb[right_child + 1, splitvar] <- tree[Node == nn, Split]
    }
  }
  list(lb = lb, ub = ub)
}


probFunctionEmp <- function(coords, lb, ub, x) {
  mean(apply(t(x[, coords]) > lb & t(x[, coords]) < ub, 2, all))
}


simulate_dat <- function(mu, sigma, n = 1e5, additional_vars = 5) {
  set.seed(1)
  p <- length(mu)
  # Simulate data
  x <- matrix(
    rmvnorm(n = n, mean = mu, sigma = sigma),
    ncol = p,
  )
  if (additional_vars > 0) {
    x <- cbind(x, matrix(runif(n * additional_vars), ncol = additional_vars))
  }
  colnames(x) <- paste0("x", seq_len(ncol(x)))

  lp <- x[, 1] + x[, 2] + 2 * x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  # Return the dataset
  list(x = x, y = y)
}
