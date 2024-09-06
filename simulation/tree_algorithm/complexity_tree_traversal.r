devtools::load_all()
source('generate_binary_tree.r')

predict_single_tree_fast <- function(tree, observation) {
  current_node <- 0 # Start from the root node

  while (TRUE) {
    # Access the current node
    node <- tree[current_node + 1, ] # +1 because R indexes from 1

    # If it's a leaf node, return the prediction
    if (node$Feature == "Leaf") {
      return(node$Quality)
    }

    # Get the feature and split value
    feature <- as.character(node$Feature)
    split_value <- as.numeric(node$Split)

    # Determine the next node based on the observation
    if (observation[feature] < split_value) {
      # Move to the 'Yes' branch (left child)
      current_node <- as.integer(node$Yes)
    } else {
      # Move to the 'No' branch (right child)
      current_node <- as.integer(node$No)
    }
  }
}

compute_marginal_expecation_tree <- function(
    x_foreground,
    dataset,
    tree,
    to_explain = c(1)) {

  mean(apply(dataset, 1, function(x) {
    x[to_explain] <- x_foreground[to_explain]
    predict_single_tree_fast(tree, x)
  }))
}

compute_marginal_expecation_tree_leaf_weights <- function(
    x_foreground,
    dataset,
    tree,
    lb, ub,
    to_explain = c(1)) {
  n_features <- ncol(dataset)
  to_int <- setdiff(1:n_features, to_explain)

  recurse <- function(node_idx) {
    node_idx <- node_idx + 1
    node <- tree[node_idx, ] # +1 because R indexes from 1
    if (node$Feature == "Leaf") {
      v <- node$Quality * probFunctionEmp(to_int, lb[node_idx, to_int], ub[node_idx, to_int], dataset)
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

