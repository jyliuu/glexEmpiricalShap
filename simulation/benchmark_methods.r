devtools::load_all()
setwd("simulation")
source("export_data.r")
source("simulate_functions.r")
source("cv.r")
library(xgboost)
library(tidyverse)

sim_dat_fit_xgboost <- function(sim_dat_fun, learner_fun, save = TRUE) {
  dataset <- sim_dat_fun()
  xg <- learner_fun(dataset)
  if (save) {
    export_data_as_csv(dataset, "/Users/sqf320/Documents/shap/simulation/data/data.csv")
    export_xgboost_model(xg, "/Users/sqf320/Documents/shap/simulation/data/model.model")
  }

  return(list(xg = xg, dataset = dataset))
}

get_splitted_features <- function(xg) {
  trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)
  all_S <- lapply(0:max(trees$Tree), function(tree) {
    trees[Tree == tree & Feature != "Leaf", sort(unique(Feature))]
  })

  all_S
}

benchmark_glex_xg_vs_N <- function(
  xg,
  dat,
  N = (21:40) * 100,
  min_iterations = 5,
  save_name = NULL
) {
  bench_res_increasing_N <- bench::press(
    N = N,
    {
      bench::mark(
        glex::glex(xg, dat[1:N, ]),
        check = FALSE,
        min_iterations = min_iterations,
        time_unit = "s"
      )
    }
  )
  if (!is.null(save_name)) {
    saveRDS(bench_res_increasing_N, save_name)
  }
  bench_res_increasing_N
}

get_dat <- function() {
  df <- import_data_from_csv("/Users/sqf320/Documents/shap/simulation/data/data.csv")
  x <- subset(df, select = -y)
  y <- df$y
  list(x = x, y = y)
}

train_learner <- function(dataset) {
  # Run CV and obtain the best learner
  cv_res <- cv_and_obtain_learner(
    dataset,
    n_evals = 1, nrounds = 20, max_depth = 5
  )
  cv_res$learner$model
}

get_learner <- function(d) {
  xg <- import_xgboost_model("/Users/sqf320/Documents/shap/simulation/data/model.model")
  xg
}
save_bench_res <- function(bench_res, save_name) {
  saveRDS(bench_res, save_name)
}

# dataset_and_xg <- sim_dat_fit_xgboost(get_dat, get_learner, save = FALSE)
dataset <- get_dat()
xg <- get_learner()

bench_res <- benchmark_glex_xg_vs_N(xg, dataset$x)
save_bench_res(bench_res, "res/bench_times_glex2100.rds")

object3 <- glex::glex(xg, dataset$x[1:100, ]) # using empirical marginals
autoplot(object3, c("x1"))

dat <- as.data.frame(dataset$x)
