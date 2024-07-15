#' Check names for the code < block < chapter
#'
#' Renames all of the entries in `all_info` vector
#' as code, block or chapter
#'
#' @param all_info A vector with code < block < chapter
#' or block < chapter or just chapter
#'
#' @returns A vector with homogenous names
#' @keywords internal
check_names <- function(all_info) {
  k <- 1
  while (k <= length(all_info)) {
    # If it is a code check whether it is a code, block or chapter
    if (grepl("code", names(all_info[k]))) {
      if (grepl("-", all_info[k])) {
        names(all_info)[k] <- "block"
        names(all_info)[k + 1] <- "block_title"
        k <- k + 1
      } else if (grepl("[0-9]", all_info[k])) {
        names(all_info)[k] <- "code"
        names(all_info)[k + 1] <- "code_title"
        k <- k + 1
      } else {
        names(all_info)[k] <- "chapter"
        names(all_info)[k + 1] <- "chapter_title"
        k <- k + 1
      }
    }
    k <- k + 1
  }

  # Change the numbering of the make-unique
  all_info <- rev(all_info)
  names(all_info) <- make.unique(names(all_info), sep = "_")
  all_info <- rev(all_info)

  return(all_info)
}
