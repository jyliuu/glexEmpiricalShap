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

augment_and_take_expectation <- function(
  x_foreground,
  dataset,
  tree,
  to_explain = c(1)
) {
  augmented_matrix <- augment_tree(tree, dataset)
  augmented_expectation_tree_traversal(
    x_foreground,
    dataset,
    tree,
    augmented_matrix,
    to_explain
  )
}
