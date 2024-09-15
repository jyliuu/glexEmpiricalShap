
augment_tree <- function(tree, dataset) {
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


augmented_expectation_tree_traversal <- function(
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
