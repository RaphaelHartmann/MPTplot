
get_categories <- function(lines) {

  tree_list <- list()
  tree_list$categories <- list()
  current_tree <- NULL

  # Iterate over the vector
  for (item in lines) {
    if (item != "") {
      # If the item is not empty, add it to the current group
      current_tree <- c(current_tree, item)
    } else {
      # If the item is empty, check if we have a non-empty group to add to the tree list
      if (length(current_tree) > 0) {
        tree_list$categories <- c(tree_list$categories, list(current_tree))
        current_tree <- NULL
      }
    }
  }

  # Add the last tree if there is one
  if (length(current_tree) > 0) {
    tree_list$categories <- c(tree_list$categories, list(current_tree))
  }

  return(tree_list)
}


get_branches <- function(tree_list) {
  for (tr in 1:length(tree_list$categories)) {
    branches_raw <- strsplit(tree_list$categories[[tr]], split = "\\+")
    tree_list$branches[[tr]] <- unlist(branches_raw)
    tree_list$cat_pos[[tr]] <- rep(seq_along(branches_raw), lengths(branches_raw))
    params_raw <- unlist(strsplit(tree_list$branches[[tr]], "\\*"))
    params <- gsub("\\(1-", "", params_raw)  # Remove "(1-"
    params <- gsub("\\)", "", params)  # Remove ")"
    tree_list$params[[tr]] <- sort(unique(params))
  }
  return(tree_list)
}

#' @importFrom stats runif
test_tree <- function(tree_list) {
  for (tr in 1:length(tree_list$categories)) {
    param_env <- new.env()
    params <- tree_list$params[[tr]]
    formulas <- tree_list$branches[[tr]]
    for (param in params) assign(param, round(runif(1, 0.2, 0.8), 2), envir = param_env)
    for (param in params) formulas <- gsub(param, as.character(get(param, envir = param_env)), formulas)
    if (round(sum(sapply(formulas, function(f) eval(parse(text = f), envir = param_env))), 10) != 1) stop(paste0("tree number ", tr, " does not sum to one."))
  }
}

#' @importFrom utils tail
order_branches <- function(tree_list) {
  for (tr in 1:length(tree_list$categories)) {
    n.branches <- length(tree_list$branches[[tr]])
    tree_list$branch_order[[tr]] <- rep(NA, n.branches)
    # get the current tree in the form of vectors
    strip_branch <- sapply(as.list(tree_list$branches[[tr]]), function (x) strsplit(x, split = "\\*"))
    # get the indices of the first and last branch in the tree
    ind_b1 <- which(sapply(strip_branch, function(x) all(!grepl("\\(1-", x))))
    tree_list$branch_order[[tr]][ind_b1] <- 1
    ind_blast <- which(sapply(strip_branch, function(x) all(grepl("\\(1-", x))))
    tree_list$branch_order[[tr]][ind_blast] <- n.branches

    # save the first tree as the current tree for the start of the while loop
    current_branch <- strip_branch[[ind_b1]]
    br = 2 # since we have the first branch, we need the second one next
    while (br != n.branches) {
      last_pos <- tail(which(!grepl("\\(1-", current_branch)), 1)
      if (last_pos == 1) {
        new_pattern <- paste0("(1-", current_branch[1], ")")
        ind_new <- which(sapply(strip_branch, function(x) {
          return(x[1] == new_pattern && all(!grepl("\\(1-", x[-1])))
        }))
      } else {
        new_pattern <- c(current_branch[1:(last_pos-1)], paste0("(1-", current_branch[last_pos], ")"))
        ind_new <- which(sapply(strip_branch, function(x) {
          return(identical(x[1:length(new_pattern)], new_pattern) && all(!grepl("\\(1-", x[-(1:length(new_pattern))])))
        }))
      }
      tree_list$branch_order[[tr]][ind_new] <- br
      current_branch <- strip_branch[[ind_new]]
      br = br + 1
    }
    tree_list$ordered_branches[[tr]] <- tree_list$branches[[tr]][order(tree_list$branch_order[[tr]])]
    tree_list$ordered_cat_pos[[tr]] <- tree_list$cat_pos[[tr]][order(tree_list$branch_order[[tr]])]
  }
  return(tree_list)
}


tree_info <- function(lines) {

  # get the categories per tree
  tree_list <- get_categories(lines)

  # count length of each list element
  tree_list$cat_count <- lapply(tree_list$categories, function(x) {
    length(x)
  })

  # get branches and parameters of each tree
  tree_list <- get_branches(tree_list)

  # test each tree sums to one
  test_tree(tree_list)

  # order branches
  tree_list <- order_branches(tree_list)

  return(tree_list)

}
