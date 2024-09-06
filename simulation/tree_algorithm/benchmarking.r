
# Example usage:
tree <- generate_binary_tree(max_depth = 3, n_features = 2)
print(tree)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
n <- 1e3
dataset <- simulate_dat(mu, sigma, n = n, additional_vars = 0)
dtrain <- xgb.DMatrix(data = dataset$x, label = dataset$y)

# Run CV and obtain the best learner
# cv_res <- cv_and_obtain_learner(dataset, n_evals = 13)
# cv_res <- cv_and_obtain_learner(dataset, nrounds = 1, n_evals = 1)
# xg <- cv_res$learner$model

params <- list(
  objective = "reg:squarederror", # Multiclass classification
  max_depth = 100, # High depth for the tree
  eta = 0.1, # Learning rate (can adjust as needed)
  nthread = 5 # Number of threads (adjust based on your system)
)
xg <- xgboost(
  data = dtrain, # Feature matrix
  params = params, # Parameters
  nrounds = 1, # Train only one tree
  verbose = 1 # Verbosity level (set to 0 for no output)
)
gr <- xgb.plot.tree(model = xg, trees = 0, render = F)
DiagrammeR::export_graph(gr, "tree.pdf")
trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)
tree_0 <- trees[Tree == 0]
tree_0[, Feature_num := as.integer(factor(Feature, levels = c("Leaf", colnames(dataset$x)))) - 1L]


obs <- data.frame(x1 = 1.99, x2 = 2)
tree <- generate_binary_tree(5)
bounds <- compute_lb_ub(tree, 2)
(prediction <- predict_single_tree_fast(tree, obs))
compute_marginal_expecation_tree(obs, as.data.frame(dataset$x), tree, to_explain = c())
compute_marginal_expecation_tree_leaf_weights(obs, as.data.frame(dataset$x), tree, bounds$lb, bounds$ub, to_explain = c())

library(bench)
to_test_d <- 12:15
to_test_trees2 <- lapply(to_test_d, function(d) {
  tree <- generate_binary_tree(d)
  bounds <- compute_lb_ub(tree, 2)
  list(tree = tree, bounds = bounds)
})

benchmarks2 <- bench::press(
  d = seq_along(to_test_trees),
  {
    bench::mark(
      naive = compute_marginal_expecation_tree(
        obs,
        as.data.frame(dataset$x),
        to_test_trees[[d]]$tree,
        to_explain = c()
      ),
      improved = compute_marginal_expecation_tree_leaf_weights(
        obs,
        as.data.frame(dataset$x),
        to_test_trees[[d]]$tree,
        to_test_trees[[d]]$bounds$lb,
        to_test_trees[[d]]$bounds$ub,
        to_explain = c()
      ),
      min_iterations = 20,
      time_unit = "ms"
    )
  }
)

# Plot using # Plot using ggplot2
ggplot(benchmarks2, aes(x = d, y = median, color = expression)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    x = "N",
    y = "Time (seconds)",
    title = "Time Complexity of Naive vs Improved Algorithms",
  ) +
  theme_minimal()
ggsave("test.png")

autoplot(benchmarks2)
