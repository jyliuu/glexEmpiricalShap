library(mlr3verse)


cv_and_obtain_learner <- function(
    dataset,
    nrounds = to_tune(1, 1000),
    eta = to_tune(0.01, 0.3),
    max_depth = to_tune(2, 6),
    n_evals = 50,
    folds = 5) {
  df <- data.frame(dataset$x, y = dataset$y)

  # Define the Task
  task <- as_task_regr(df, id = "my_task", target = "y")

  # Initialize the Learner
  learner <- lrn("regr.xgboost",
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth,
    objective = "reg:squarederror"
  )

  # Create a Tuner
  tuner <- tnr("random_search")

  # Tune the Model
  instance <- ti(
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = folds),
    measures = msr("regr.mse"),
    terminator = trm("evals", n_evals = n_evals)
  )

  tuner$optimize(instance)

  learner$param_set$values <- instance$result_learner_param_vals
  learner$train(task)

  list(learner = learner, instance = instance, task = task)
}

cv_and_obtain_learner_ranger <- function(
    dataset,
    num_trees = to_tune(1, 300), # Number of trees
    min_node_size = to_tune(1, 6), # Minimum node size
    mtry = to_tune(1, floor(sqrt(ncol(dataset$x)))), # Number of variables to possibly split at in each node
    sample_fraction = to_tune(0.5, 0.8), # Proportion of observations to sample for each tree
    n_evals = 50,
    folds = 5) {
  df <- data.frame(dataset$x, y = dataset$y)

  # Define the Task
  task <- as_task_regr(df, target = "y")

  # Initialize the Learner
  learner <- lrn("regr.ranger",
    num.trees = num_trees,
    min.node.size = min_node_size,
    mtry = mtry,
    sample.fraction = sample_fraction,
    node.stats = TRUE
  )

  # Create a Tuner
  tuner <- tnr("random_search")

  instance <- ti(
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = folds),
    measures = msr("regr.mse"),
    terminator = trm("evals", n_evals = n_evals)
  )

  tuner$optimize(instance)

  learner$param_set$values <- instance$result_learner_param_vals
  learner$train(task)

  list(learner = learner, instance = instance, task = task)
}
