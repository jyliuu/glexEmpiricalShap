devtools::load_all()
source("export_data.r")
source("simulate_functions.r")
source("cv.r")
library(xgboost)
library(tidyverse)

sim_dat_fit_xgboost <- function(sim_dat_fun, learner_fun, save = TRUE) {
  dataset <- sim_dat_fun()
  xg <- learner_fun(dataset)
  if (save) {
    export_data_as_csv(dataset, "simulation_depends/data.csv")
    export_xgboost_model(xg, "simulation_depends/model.model")
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
  N = (1:5) * 100,
  min_iterations = 55,
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
  df <- import_data_from_csv("simulation_depends/data.csv")
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
  xg <- import_xgboost_model("simulation_depends/model.model")
  xg
}
save_bench_res <- function(bench_res, save_name) {
  saveRDS(bench_res, save_name)
}

# dataset_and_xg <- sim_dat_fit_xgboost(get_dat, train_learner, save = FALSE)
# dataset <- dataset_and_xg$dataset
# xg <- dataset_and_xg$xg
dataset <- get_dat()
xg <- get_learner()


# Function to run the benchmark for a given i
run_benchmark <- function(i) {
  benchmark_glex_xg_vs_N(
    xg, dataset$x,
    N = i * 1000,
    min_iterations = 300,
    save_name = paste0("bench_times_glex", i, ".rds")
  )
}


args <- commandArgs(trailingOnly = TRUE)

# Extract the first argument (the parameter value)
param <- as.numeric(args[1])

# Your R code here using 'param'
print(paste("Running with parameter:", param))

run_benchmark(param)
