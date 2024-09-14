augment_tree <- function(tree, dataset, n_features = 2) {
  PathData <- NULL
  Feature <- NULL

  augmented <- tree[Feature == "Leaf", "Node"]
  augmented[, PathData := replicate(.N, replicate(n_features + 1, numeric(0), simplify = FALSE), simplify = FALSE)]
  setkey(augmented, Node)


  recurse <- function(node_idx, passed_down) {
    node_idx <- node_idx + 1
    current_node <- tree[node_idx, ]
    if (current_node$Feature == "Leaf") {
      augmented[Node == current_node$Node, PathData := list(passed_down)]
      return()
    }

    passed_down_yes <- replicate(n_features + 1, numeric(0), simplify = FALSE)
    passed_down_no <- replicate(n_features + 1, numeric(0), simplify = FALSE)

    current_feature <- current_node$Feature_num + 1
    split <- current_node$Split
    passed_down_yes[[current_feature]] <- passed_down[[current_feature]]
    passed_down_no[[current_feature]] <- passed_down[[current_feature]]

    passed_down_1 <- passed_down[[1]]

    passed_down_yes[[1]] <- passed_down[[1]][dataset[passed_down_1, current_feature - 1] < split]
    passed_down_no[[1]] <- setdiff(passed_down_1, passed_down_yes[[1]])

    for (s in setdiff(1:n_features, current_feature)) {
      passed_down_s <- passed_down[[s]]
      passed_down_yes[[s]] <- passed_down_s[dataset[passed_down_s, current_feature - 1] < split]
      passed_down_no[[s]] <- setdiff(passed_down_s, passed_down_yes[[s]])
    }

    recurse(current_node$Yes, passed_down_yes)
    recurse(current_node$No, passed_down_no)
  }

  to_pass_down <- replicate(n_features + 1, seq_len(nrow(dataset)), simplify = FALSE)

  recurse(0, to_pass_down)
  return(augmented)
}


augmented_expectation_tree_traversal <- function(
    x_foreground,
    dataset,
    tree,
    augmented_matrix,
    to_explain = c(1)) {
  if (is.null(to_explain)) to_explain <- 0

  n_features <- ncol(dataset)
  n <- nrow(dataset)

  recurse <- function(node_idx) {
    node_idx <- node_idx + 1
    node <- tree[node_idx, ] # +1 because R indexes from 1
    if (node$Feature == "Leaf") {
      path_data <- augmented_matrix[Node == node$Node, PathData][[1]]
      v <- node$Quality * length(Reduce(intersect, path_data[to_explain + 1])) / n
      return(v)
    }

    # Get the feature and split value
    feature <- node$Feature_num
    split_value <- node$Split

    # Determine the next node based on the observation
    if (feature %in% to_explain) {
      if (x_foreground[feature] < split_value) {
        # Move to the 'Yes' branch (left child)
        return(recurse(node$Yes))
      } else {
        # Move to the 'No' branch (right child)
        return(recurse(node$No))
      }
    } else {
      return(recurse(node$Yes) + recurse(node$No))
    }
  }
  recurse(0)
}


augment_tree2 <- function(tree, dataset, n_features = 2) {
  PathData <- NULL
  Feature <- NULL

  augmented <- tree[Feature == "Leaf", "Node"]
  augmented[, PathData := replicate(.N, list(matrix(T, nrow = n_features + 1, ncol = nrow(dataset))), simplify = FALSE)]
  setkey(augmented, Node)

  recurse <- function(node_idx, passed_down) {
    node_idx <- node_idx + 1
    current_node <- tree[node_idx, ]
    if (current_node$Feature == "Leaf") {
      augmented[Node == current_node$Node, PathData := list(passed_down)]
      return()
    }
    to_pass_down_yes <- passed_down
    to_pass_down_no <- passed_down

    current_feature <- current_node$Feature_num + 1
    split <- current_node$Split

    curr_pass <- passed_down[current_feature, ]
    yes_obs <- dataset[curr_pass, current_feature - 1] < split
    no_obs <- !yes_obs

    to_pass_down_yes[current_feature, curr_pass] <- yes_obs
    to_pass_down_no[current_feature, curr_pass] <- no_obs

    curr_pass_0 <- passed_down[1, ]
    yes_obs_0 <- dataset[curr_pass_0, current_feature - 1] < split
    no_obs_0 <- !yes_obs_0

    to_pass_down_yes[1, curr_pass_0] <- yes_obs_0
    to_pass_down_no[1, curr_pass_0] <- no_obs_0

    recurse(current_node$Yes, to_pass_down_yes)
    recurse(current_node$No, to_pass_down_no)
  }

  to_pass_down <- matrix(T, nrow = n_features + 1, ncol = nrow(dataset))

  recurse(0, to_pass_down)
  return(augmented)
}

get_all_subsets <- function(vec) {
  # Get all subsets for each possible size
  subsets <- lapply(seq_along(vec),
    function(x) lapply(combn(vec, x, simplify = FALSE), sort)
  )
  # Flatten the list of lists into a single list
  subsets <- unlist(subsets, recursive = FALSE)
  return(subsets)
}


augmented_expectation_tree_traversal2 <- function(
  x_foreground,
  dataset,
  tree,
  augmented_data,
  to_explain = c(1)
) {
  if (is.null(to_explain)) to_explain <- 0

  n_features <- ncol(dataset)
  n <- nrow(dataset)

  recurse <- function(node_idx) {
    node_idx <- node_idx + 1
    node <- tree[node_idx, ] # +1 because R indexes from 1
    if (node$Feature == "Leaf") {
      path_data <- augmented_data[Node == node$Node, PathData][[1]]
      selected_obs <- path_data[setdiff(2:(n_features + 1), to_explain + 1), ]
      if (is.matrix(selected_obs)) selected_obs <- apply(selected_obs, 2, all)
      v <- node$Quality * mean(selected_obs)
      return(v)
    }

    # Get the feature and split value
    feature <- node$Feature_num
    split_value <- node$Split

    # Determine the next node based on the observation
    if (feature %in% to_explain) {
      if (x_foreground[feature] < split_value) {
        # Move to the 'Yes' branch (left child)
        return(recurse(node$Yes))
      } else {
        # Move to the 'No' branch (right child)
        return(recurse(node$No))
      }
    } else {
      return(recurse(node$Yes) + recurse(node$No))
    }
  }
  recurse(0)
}


augment_tree3 <- function(tree, dataset, n_features = 2) {
  PathData <- NULL
  Feature <- NULL

  augmented <- tree[Feature == "Leaf", "Node"]
  augmented[, PathData := replicate(.N, list(), simplify = FALSE)]
  setkey(augmented, Node)

  recurse <- function(node_idx, passed_down) {
    node_idx <- node_idx + 1
    current_node <- tree[node_idx, ]
    if (current_node$Feature == "Leaf") {
      subsets <- get_all_subsets(passed_down$encountered)
      for (subset in subsets) {
        subset_name <- paste0(subset, collapse = "-")
        passed_down$encountered <- c(passed_down$encountered)
        passed_down$mat <- rbind(passed_down$mat, apply(passed_down$mat[subset, ], 2, all))
        passed_down$indices[[subset_name]] <- nrow(passed_down$mat)
      }

      augmented[Node == current_node$Node, PathData := list(passed_down)]
      return()
    }

    if (!(current_node$Feature_num %in% passed_down$encountered)) {
      passed_down$encountered <- c(passed_down$encountered, current_node$Feature_num)
      passed_down$mat <- rbind(passed_down$mat, matrix(TRUE, nrow = 1, ncol = nrow(dataset)))
      passed_down$indices[[as.character(current_node$Feature_num)]] <- nrow(passed_down$mat)
    }

    to_pass_down_yes <- copy(passed_down)
    to_pass_down_no <- copy(passed_down)

    current_feature_num <- current_node$Feature_num
    c_feat_idx <- passed_down$indices[[as.character(current_feature_num)]]
    split <- current_node$Split

    curr_pass <- passed_down$mat[c_feat_idx, ]
    yes_obs <- dataset[curr_pass, current_feature_num] < split
    no_obs <- !yes_obs

    to_pass_down_yes$mat[c_feat_idx, curr_pass] <- yes_obs
    to_pass_down_no$mat[c_feat_idx, curr_pass] <- no_obs

    curr_pass_0 <- passed_down$mat[1, ]
    yes_obs_0 <- dataset[curr_pass_0, current_feature_num] < split
    no_obs_0 <- !yes_obs_0

    to_pass_down_yes$mat[1, curr_pass_0] <- yes_obs_0
    to_pass_down_no$mat[1, curr_pass_0] <- no_obs_0

    recurse(current_node$Yes, to_pass_down_yes)
    recurse(current_node$No, to_pass_down_no)
  }

  to_pass_down <- list(
    mat = matrix(TRUE, nrow = 1, ncol = nrow(dataset)),
    encountered = numeric(),
    indices = list("0" = 1)
  )

  recurse(0, to_pass_down)
  return(augmented)
}


augmented_expectation_tree_traversal3 <- function(
  x_foreground,
  dataset,
  tree,
  augmented_data,
  to_explain = c(1)
) {
  if (is.null(to_explain)) to_explain <- 0

  n_features <- ncol(dataset)
  n <- nrow(dataset)

  recurse <- function(node_idx) {
    node_idx <- node_idx + 1
    node <- tree[node_idx, ] # +1 because R indexes from 1
    if (node$Feature == "Leaf") {
      path_data <- augmented_data[Node == node$Node, PathData][[1]]
      to_marginalize <- sort(setdiff(path_data$encountered, to_explain))

      if (length(to_marginalize) == 0)
        return(node$Quality)

      idx_name <- paste(to_marginalize, collapse = "-")
      idx <- path_data$indices[[idx_name]]
      selected_obs <- path_data$mat[idx, ]

      v <- node$Quality * mean(selected_obs)
      return(v)
    }

    # Get the feature and split value
    feature <- node$Feature_num
    split_value <- node$Split

    # Determine the next node based on the observation
    if (feature %in% to_explain) {
      if (x_foreground[feature] < split_value) {
        # Move to the 'Yes' branch (left child)
        return(recurse(node$Yes))
      } else {
        # Move to the 'No' branch (right child)
        return(recurse(node$No))
      }
    } else {
      return(recurse(node$Yes) + recurse(node$No))
    }
  }
  recurse(0)
}

augment_tree4 <- function(tree, dataset) {
  PathData <- NULL
  Feature <- NULL

  augmented <- tree[Feature == "Leaf", "Node"]
  augmented[, PathData := replicate(.N, list(), simplify = FALSE)]
  setkey(augmented, Node)


  recurse <- function(node_idx, passed_down) {
    node_idx <- node_idx + 1
    current_node <- tree[node_idx, ]
    if (current_node$Feature == "Leaf") {
      augmented[Node == current_node$Node, PathData := list(passed_down)]
      return()
    }

    passed_down_yes <- copy(passed_down)
    passed_down_no <- copy(passed_down)

    current_feature <- current_node$Feature_num
    split <- current_node$Split

    for (subset in names(passed_down$features_map)) {
      current_subset <- passed_down$features_map[[subset]]
      current_path_dat <- passed_down$path_dat[[subset]]
      if (current_feature %in% current_subset) {
        passed_down_yes$path_dat[[subset]] <- current_path_dat
        passed_down_no$path_dat[[subset]] <- current_path_dat
      } else {
        passed_down_yes$path_dat[[subset]] <- current_path_dat[dataset[current_path_dat, current_feature] < split]
        passed_down_no$path_dat[[subset]] <- setdiff(current_path_dat, passed_down_yes$path_dat[[subset]])
      }
    }

    if (!(current_feature %in% passed_down$encountered)) {
      passed_down_yes$encountered <- c(passed_down$encountered, current_feature)
      passed_down_no$encountered <- passed_down_yes$encountered

      for (subset_name in names(passed_down$features_map)) {
        subset <- passed_down$features_map[[subset_name]]
        to_add <- sort(c(subset, current_feature))
        to_add_name <- paste0(to_add, collapse = "-")

        passed_down_yes$features_map[[to_add_name]] <- to_add
        passed_down_no$features_map[[to_add_name]] <- to_add

        passed_down_yes$path_dat[[to_add_name]] <- passed_down$path_dat[[subset_name]]
        passed_down_no$path_dat[[to_add_name]] <- passed_down$path_dat[[subset_name]]
      }
    }

    recurse(current_node$Yes, passed_down_yes)
    recurse(current_node$No, passed_down_no)
  }

  to_pass_down <- list(
    path_dat = list("0" = seq_len(nrow(dataset))),
    encountered = numeric(),
    features_map = list("0" = numeric())
  )

  recurse(0, to_pass_down)
  return(augmented)
}


augmented_expectation_tree_traversal4 <- function(
    x_foreground,
    dataset,
    tree,
    augmented_matrix,
    to_explain = c(1)) {
  if (is.null(to_explain)) to_explain <- 0

  n <- nrow(dataset)

  recurse <- function(node_idx) {
    node_idx <- node_idx + 1
    node <- tree[node_idx, ] # +1 because R indexes from 1
    if (node$Feature == "Leaf") {
      path_data <- augmented_matrix[Node == node$Node, PathData][[1]]

      to_marginalize <- paste0(
        sort(intersect(path_data$encountered, to_explain)),
        collapse = "-"
      )
      if (to_marginalize == "")
        to_marginalize <- "0"

      v <- node$Quality * length(path_data$path_dat[[to_marginalize]]) / n
      return(v)
    }

    # Get the feature and split value
    feature <- node$Feature_num
    split_value <- node$Split

    # Determine the next node based on the observation
    if (feature %in% to_explain) {
      if (x_foreground[feature] < split_value) {
        # Move to the 'Yes' branch (left child)
        return(recurse(node$Yes))
      } else {
        # Move to the 'No' branch (right child)
        return(recurse(node$No))
      }
    } else {
      return(recurse(node$Yes) + recurse(node$No))
    }
  }
  recurse(0)
}


augment_and_take_expectation <- function(
    x_foreground,
    dataset,
    tree,
    to_explain = c(1),
    augment_method = augment_tree4,
    expectation_method = augmented_expectation_tree_traversal4) {
  augmented_matrix <- augment_method(tree, dataset)
  expectation_method(
    x_foreground,
    dataset,
    tree,
    augmented_matrix,
    to_explain
  )
}
