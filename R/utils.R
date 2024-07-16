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

#' Function to order the column names of a searched data frame
#'
#' @param df A data frame with columns `level_0`, `title_0`, `level_1`, `title_1`, etc.
#' @return The dataframe with the columns ordered according to level number.
#' @keywords internal
order_columns <- function(df){
  # Extract column names
  col_names <- colnames(df)

  # Function to extract numeric value from column names
  extract_number <- function(name) {
    as.numeric(sub(".*_(\\d+)", "\\1", name))
  }

  # Split column names into "level" and "title" components
  column_names <- col_names[grep("^level|title_", col_names)]

  # Sort column names based on the extracted numeric value
  sorted_col_names <- column_names[order(sapply(column_names, extract_number))]

  return(df[, sorted_col_names])
}


#' Function to right align the data.frame of chapters
#'
#' Right aligns the search data frame so that it has the correct levels
#' @inheritParams order_columns
#'
#' @return A right-aligned `data.frame`
#' @keywords internal
right_align_df <- function(df) {

  # Get the number of rows and columns
  n_rows <- nrow(df)
  n_cols <- ncol(df)

  # Create a new empty dataframe with the same dimensions
  new_df <- as.data.frame(matrix(NA_character_, nrow = n_rows, ncol = n_cols))
  colnames(new_df) <- colnames(df)

  for (i in 1:n_rows) {

    if (all(is.na(df[i,2:n_cols]))){

      new_df[i, 1:n_cols] <- NA_character_

    } else {

      # Extract the non-NA values from the current row
      non_na_values <- df[i, !is.na(df[i, ])]

      # Determine the starting position for the non-NA values in the new dataframe
      if (length(non_na_values) > 0){
        start_pos <- n_cols - length(non_na_values) + 1

        # Assign the non-NA values to the right-aligned positions in the new dataframe
        new_df[i, start_pos:n_cols] <- non_na_values
      }
    }
  }

  return(new_df)
}

#' Function to add names to chapters / blocks to df
#'
#' Adds the appropriate names to chapters, blocks and codes
#' @inheritParams order_columns
#'
#' @return A named `data.frame`
#' @keywords internal
name_df <- function(df) {

  all_names <- c("code_specific", "code_specific_title", "code", "code_title",
                 "block", "block_title", "chapter", "chapter_title")

  ncols <- ncol(df)
  if (ncols == 8){
    colnames(df) <- all_names
  } else if (ncols == 6){
    colnames(df) <- all_names[3:length(all_names)]
  } else if (ncols == 4){
    colnames(df) <- all_names[5:length(all_names)]
  } else if (ncols == 2) {
    colnames(df) <- all_names[7:length(all_names)]
  }

  return(df)
}
