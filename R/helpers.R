# Make a phylo object safe to use/write:
# - add edge lengths if missing
# - remove node labels if their length is invalid
normalize_phylo <- function(tr) {
  if (!inherits(tr, "phylo")) return(tr)

  if (is.null(tr$edge.length) || length(tr$edge.length) == 0) {
    tr$edge.length <- rep(1, nrow(tr$edge))
  }

  if (!is.null(tr$node.label) && length(tr$node.label) != tr$Nnode) {
    tr$node.label <- NULL
  }

  tr
}

safe_extract <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)
}

safe_write_tree <- function(tr) {
  tr <- normalize_phylo(tr)
  ape::write.tree(tr)
}

assert_single_phylo <- function(tree, arg = "file") {
  if (inherits(tree, "multiPhylo")) {
    stop(
      "`", arg, "` contains multiple trees. Use calculate_all_subtree_indices() or process_tree_folder() for multiPhylo inputs.",
      call. = FALSE
    )
  }

  if (!inherits(tree, "phylo")) {
    stop("`", arg, "` must be a phylo object after conversion.", call. = FALSE)
  }

  tree
}
