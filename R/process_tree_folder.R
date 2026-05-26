#' Process all tree files in a folder
#'
#' This function searches a folder for tree files, calculates subtree indices
#' for each tree file, and exports one CSV file per input tree file.
#'
#' @param tree_folder Folder containing tree files.
#' @param min_tips Minimum number of tips for subtrees to include.
#' @param max_tips Maximum number of tips for subtrees to include.
#' @param include_full_tree Logical. Should the full tree also be included?
#' @param pattern File extension pattern used to find tree files.
#' @param output_folder Folder where CSV files should be saved. Defaults to
#'   `tree_folder`.
#'
#' @return Invisibly returns a list of result data frames, one per input tree
#'   file.
#'
#' @export
process_tree_folder <- function(
  tree_folder,
  min_tips = 0,
  max_tips = 20000,
  include_full_tree = FALSE,
  pattern = "\\.(txt|nwk|tree|tre|nex|nexus)$",
  output_folder = tree_folder
) {

  if (!dir.exists(tree_folder)) {
    stop("`tree_folder` does not exist: ", tree_folder, call. = FALSE)
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  }

  tree_files <- list.files(
    path = tree_folder,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  message("Found ", length(tree_files), " tree files")

  all_results <- lapply(tree_files, function(f) {
    tryCatch(
      calculate_all_subtree_indices(
        file = f,
        min_tips = min_tips,
        max_tips = max_tips,
        include_full_tree = include_full_tree
      ),
      error = function(e) {
        warning(paste("Skipping file", basename(f), "because:", e$message))
        return(NULL)
      }
    )
  })

  for (i in seq_along(tree_files)) {
    if (is.null(all_results[[i]]) || nrow(all_results[[i]]) == 0) next

    input_name <- tools::file_path_sans_ext(basename(tree_files[i]))
    output_csv <- file.path(output_folder, paste0(input_name, ".csv"))

    utils::write.csv(all_results[[i]], output_csv, row.names = FALSE)
    message("CSV exported to: ", output_csv)
  }

  invisible(all_results)
}
