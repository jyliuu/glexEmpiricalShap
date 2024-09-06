devtools::load_all()
setwd("simulation")
source("simulate_functions.r")
source("plot_functions.r")
source("cv.r")
library(future)
library(ranger)
plan(multicore)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
dataset <- simulate_dat(mu, sigma, n = 1e4)
dtrain <- xgb.DMatrix(data = dataset$x, label = dataset$y)

# Run CV and obtain the best learner
# cv_res <- cv_and_obtain_learner(dataset, n_evals = 13)
cv_res <- cv_and_obtain_learner(dataset, n_evals = 1)
xg <- cv_res$learner$model

probFunction_ <- function(coords, lb, ub, cov_args) {
  pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

# True probability function
probFunction <- function(...) probFunction_(..., cov_args)

object3 <- glex::glex(xg, dataset$x) # using empirical marginals

# object1 <- glex::glex(xg, dataset$x, probFunction = probFunction) # using true marginal probs
# object2 <- glex::glex(xg, dataset$x, probFunction = "path-dependent") # using path dependent probs
# object3 <- glex::glex(xg, dataset$x, probFunction = "empirical") # using empirical marginals
# object3 <- glex::glex(xg, dataset$x, probFunction = "www") # using empirical marginals

plot_components_and_true_m <- function(o1, o2, dataset, fun = function(x) -x - 2 * 0.3) {
  ggplot(data = as.data.frame(dataset$x)) +
    geom_line(aes(x = x1, y = o1$m$x1), color = "blue") + # Line for y1 from df1
    geom_line(aes(x = x1, y = o2$m$x1), color = "red") + # Line for y2 from df2
    stat_function(
      fun = fun, # Replace 'your_function'
      aes(x = x1), color = "black", size = 1
    ) + # with your actual function
    labs(x = "x", y = "y", title = "Plot of y1 vs y2 with Function")
}
# plots

plot_components_and_true_m(object3, object1, dataset, function(x) x- 2 * 0.3)

plot_shap(object1, object2, 1)
plot_shap(object1, object3, 1)

plot_shap(object1, object3, 2)
plot_shap(object1, object2, 2)


plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")

plot_components(object3, object1, coords = "x1")
plot_components(object3, object1, coords = "x2")

plot_components(object1, object2, coords = c("x1", "x2"))
plot_components(object3, object2, coords = c("x1", "x2"))
