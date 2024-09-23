devtools::load_all()
setwd("simulation")
source("simulate_functions.r")
source("plot_functions.r")
source("cv.r")
source("tree_algorithm/utils.r")
source("export_data.r")
library(randomPlantedForest)
library(xgboost)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
dataset <- simulate_dat(mu, sigma, n = 1e6, additional_vars = 0)
dtrain <- xgb.DMatrix(data = dataset$x, label = dataset$y)

# export_data_as_csv(dataset, "/Users/sqf320/Documents/shap/simulation/data/data.csv")

# Run CV and obtain the best learner
# cv_res <- cv_and_obtain_learner(dataset, n_evals = 13)
cv_res <- cv_and_obtain_learner(dataset, n_evals = 1, nrounds = 5)
xg <- cv_res$learner$model
xg_rpf <- convert_xgboost_rpf(xg, dataset$x, dataset$y)
# preds <- predict(xg, dataset$x)

# export_xgboost_model(xg, "/Users/sqf320/Documents/shap/simulation/data/model.model")

dat <- as.data.frame(dataset$x)
object1 <- glex::glex(xg, dat[1:1000, ], probFunction = "empirical") # using empirical marginals
object2 <- glex::glex(xg_rpf, dat[1:1000, ]) # using empirical marginals
object3 <- glex::glex(xg, dat[1:1000,]) # using empirical marginals

purify_and_call_glex <- function(xg, datx, daty){
  xg_rpf <- convert_xgboost_rpf(xg, datx, daty)
  glex::glex(xg_rpf, datx)
}

purify_and_call_glex(xg, data.frame(dataset$x[1:1000, ]), dataset$y[1:1000])

bench_res_increasing_N <- bench::press(
  n = c(800, 900),
  {
    dat <- as.data.frame(dataset$x)
    bench::mark(
      glex::glex(xg, dat[1:n, ]),
      check = FALSE,
      min_iterations = 30,
      time_unit = "s"
    )
  }
)


autoplot(bench_res_increasing_N)
ggplot(bench_res, aes(x = N, y = mem_alloc, color = as.character(expression))) +
  geom_point() +
  geom_line(aes(group = as.character(expression))) +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(
    x = "N",
    y = "Time (s)",
    title = "Time complexity for N = background = foreground",
  ) +
  theme_minimal()


plot_components_and_true_m <- function(o1, dataset, fun = function(x) -x - 2 * 0.3) {
  ggplot(data = as.data.frame(dataset$x)) +
    geom_line(aes(x = x1, y = o1$m$x1), color = "blue") + # Line for y1 from df1
    stat_function(
      fun = fun, # Replace 'your_function'
      aes(x = x1), color = "black", size = 1
    ) + # with your actual function
    labs(x = "x", y = "y", title = "Plot of y1 vs y2 with Function")
}
# plots

# plot_components_and_true_m(object3, dataset, function(x) x- 2 * 0.3)