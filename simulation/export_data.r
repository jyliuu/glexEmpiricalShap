library(xgboost)

export_xgboost_model <- function(model, file_path) {
  # Save the xgboost model to a file
  xgb.save(model, fname = file_path)

  # Print a success message
  cat("XGBoost model exported successfully to", file_path, "\n")
}

import_xgboost_model <- function(file_path) {
  # Load the xgboost model from a file
  model <- xgb.load(file_path)

  # Print a success message
  cat("XGBoost model imported successfully from", file_path, "\n")

  return(model)
}

export_data_as_csv <- function(dataset, file_path) {
  # Save the data to a CSV file
  data <- cbind(dataset$x, y = dataset$y) 
  write.csv(data, file = file_path, row.names = FALSE)

  # Print a success message
  cat("Data exported successfully to", file_path, "\n")
}

import_data_from_csv <- function(file_path, nrows = 2000) {
  # Load the data from a CSV file
  data <- read.csv(file_path, nrows=nrows)
  data
}
