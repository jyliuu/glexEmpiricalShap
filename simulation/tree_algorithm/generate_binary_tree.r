
library(data.table)
generate_binary_tree <- function(max_depth, n_features = 2, seed = 123) {
  data <- list()
  features <- paste0("x", seq_len(n_features))
  set.seed(seed)
  generate_nodes <- function(node, depth, new_bounds) {
    if (depth == max_depth) {
      data[[length(data) + 1]] <<- list(
        Tree = 0,
        Node = node,
        Quality = runif(1),
        Feature = "Leaf",
        Feature_num = NA_integer_,
        Split = NA_real_,
        Yes = NA_integer_,
        No = NA_integer_,
        Leaf = TRUE
      )
    } else {
      feature <- sample(1:n_features, 1)
      split <- runif(1, new_bounds$lb[feature], new_bounds$ub[feature])

      yes_bounds <- copy(new_bounds)
      no_bounds <- copy(new_bounds)

      yes_bounds$ub[feature] <- split
      no_bounds$lb[feature] <- split

      data[[length(data) + 1]] <<- list(
        Tree = 0,
        Node = node,
        Quality = runif(1),
        Feature = features[feature],
        Feature_num = feature,
        Split = split,
        Yes = 2 * node + 1,
        No = 2 * node + 2,
        Leaf = FALSE
      )

      generate_nodes(2 * node + 1, depth + 1, yes_bounds)
      generate_nodes(2 * node + 2, depth + 1, no_bounds)
    }
  }

  generate_nodes(0, 0, list(lb = rep(-2, n_features), ub = rep(2, n_features)))

  dt <- rbindlist(data)
  setorder(dt, Node) # Sort by Node
  dt[, Index := Node + 1] # Add Index column as Node + 1
  setcolorder(dt, c("Index", "Tree", "Node", "Quality", "Feature", "Feature_num", "Split", "Yes", "No", "Leaf"))

  return(dt)
}

simulate_dat <- function(mu, sigma, n = 1e5, additional_vars = 5) {
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