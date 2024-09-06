setwd("simulation/tree_algorithm")
devtools::load_all()

source("generate_binary_tree.r")
source("utils.r")
source("tree_traversal_speedup.r")
source("complexity_tree_traversal.r")


tree <- generate_binary_tree(3)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
n <- 1e3
dataset <- simulate_dat(mu, sigma, n = n, additional_vars = 0)
augmented <- augment_tree(tree, dataset$x)

obs <- data.frame(x1 = 1.99, x2 = 2)

(sped_up <- augmented_expectation_tree_traversal(
  obs,
  dataset$x,
  tree,
  augmented,
  to_explain = c(1)
))

augment_and_take_expectation(
  obs,
  as.data.frame(dataset$x),
  tree,
  to_explain = c(1)
)

compute_marginal_expecation_tree(
  obs,
  dataset$x,
  tree,
  to_explain = c(1)
)
bounds <- compute_lb_ub(tree, 2)

aug_row <- augmented[11, ]
idx <- tree[Node == aug_row$Node, Index]

(dataset_idx <- aug_row[, PathData][[1]][[2]])
bounds$lb[idx, 1] < dataset$x[dataset_idx, 1] & dataset$x[dataset_idx, 1] < bounds$ub[idx, 1]
which(bounds$lb[idx, 1] < dataset$x[, 1] & dataset$x[, 1] < bounds$ub[idx, 1])

compute_marginal_expecation_tree_leaf_weights(obs, as.data.frame(dataset$x), tree, bounds$lb, bounds$ub)

## Benhcmark

library(bench)
to_test_d <- 1:9
to_test_trees <- lapply(to_test_d, function(d) {
  tree <- generate_binary_tree(d)
  bounds <- compute_lb_ub(tree, 2)
  list(tree = tree, bounds = bounds)
})
augmented_matrices <- lapply(to_test_trees, function(t) augment_tree(t$tree, dataset$x))


benchmarks3 <- bench::press(
  d = seq_along(to_test_trees),
  {
    bench::mark(
      naive = compute_marginal_expecation_tree(
        obs,
        as.data.frame(dataset$x),
        to_test_trees[[d]]$tree,
        to_explain = c()
      ),
      mine = compute_marginal_expecation_tree_leaf_weights(
        obs,
        as.data.frame(dataset$x),
        to_test_trees[[d]]$tree,
        to_test_trees[[d]]$bounds$lb,
        to_test_trees[[d]]$bounds$ub,
        to_explain = c()
      ),
      niklas = augmented_expectation_tree_traversal(
        obs,
        as.data.frame(dataset$x),
        to_test_trees[[d]]$tree,
        augmented_matrix = augmented_matrices[[d]],
        to_explain = c()
      ),
      min_iterations = 20,
      time_unit = "ms"
    )
  }
)

# Plot using # Plot using ggplot2
ggplot(benchmarks3, aes(x = d, y = median, color = expression)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    x = "Depth of maximal tree",
    y = "Time (ms)",
    title = "Time complexity for a fixed number of observations and maximal tree",
  ) +
  theme_minimal()
ggsave("test2.pdf")


datasets <- lapply(10^(2:5), function(n) simulate_dat(mu, sigma, n = n, additional_vars = 0))
tree <- generate_binary_tree(9)
bounds <- compute_lb_ub(tree, 2)
augmented_matrices_new <- lapply(datasets, function(dt) augment_tree(tree, dt$x))


benchmarks_vsN <- bench::press(
  N = 1:4,
  {
    bench::mark(
      naive = compute_marginal_expecation_tree(
        obs,
        as.data.frame(datasets[[N]]$x),
        tree,
        to_explain = c()
      ),
      mine = compute_marginal_expecation_tree_leaf_weights(
        obs,
        as.data.frame(datasets[[N]]$x),
        tree,
        bounds$lb,
        bounds$ub,
        to_explain = c()
      ),
      niklas = augmented_expectation_tree_traversal(
        obs,
        as.data.frame(datasets[[N]]$x),
        tree,
        augmented_matrix = augmented_matrices_new[[N]],
        to_explain = c()
      ),
      min_iterations = 20,
      time_unit = "ms"
    )
  }
)

ggplot(benchmarks_vsN, aes(x = 10^N, y = median, color = expression)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    x = "Number of observations",
    y = "Time (ms)",
    title = "Time complexity over N for a maximal tree of depth 8",
  ) +
  theme_minimal()
ggsave("testN.pdf")