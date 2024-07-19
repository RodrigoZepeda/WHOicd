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

#' Generate a duration period for the DORIS system in ISO 8601 format
#'
#' Creates a duration period for the DORIS system in ISO-8601 format
#' from inputting years, months, weeks, days, hours, minutes, and seconds
#'
#' @param years Number of years in the duration (default = 0).
#' @param months Number of months in the duration (default = 0).
#' @param weeks Number of weeks in the duration (default = 0).
#' @param days Number of days in the duration (default = 0).
#' @param hours Number of hours in the duration (default = 0).
#' @param minutes Number of minutes in the duration (default = 0).
#' @param seconds Number of seconds in the duration (default = 0).
#'
#' @return A time duration formatted according to ISO-8601
#' @examples
#'
#' #A duration with 24 years
#' iso_8601(years = 24)
#'
#' #A duration with 6 months, 2 weeks and 5 days
#' iso_8601(months = 6, weeks = 2, days = 5)
#'
#' @export
iso_8601 <- function(years = 0, months = 0, weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 0){

  paste0("P", floor(years), "Y", floor(months), "M", floor(weeks), "W", floor(days), "D",
         "T", floor(hours), "H", floor(minutes), "M", floor(seconds), "S")

}

#' Recodify sex
#'
#' Gives the codification for sex as Male, Female or Unknown according to DORIS rules
#'
#' @inheritParams doris
#'
#' @return Sex codified as `1`, `2` or `9`
#' @keywords internal
codify_sex <- function(sex = c("Males","Females","Unknowns")){

  if (is.null(sex)){
    return(sex)
  } else if (is.numeric(sex) && (floor(sex) %in% c(1,2,9))){
    return(floor(sex))
  } else if (is.numeric(sex) && !(floor(sex) %in% c(1,2,9))) {
    stop("Invalid sex. Must be 1 for Males, 2 for Females or 9 for Unknown")
  } else if (is.character(sex)){
    sex <- switch(sex,
                  "Male" = 1,
                  "Female" = 2,
                  "Unknown" = 9)
    return(sex)
  } else {
    stop("Invalid sex")
  }
}

#' Recodify yes/no
#'
#' Gives the codification for a yes/no rule according to DORIS codification
#'
#' @param ans Codification for an answer as either numeric or text `0` (No), `1` (Yes) or
#' `9` (Unknown)
#'
#' @return Answer codified as `0` (No), `1` (Yes) or `9` (Unknown)
#' @keywords internal
codify_ans <- function(ans = c("No","Yes","Unknowns")){

  if (is.null(ans)){
    return(ans)
  } else if (is.numeric(ans) && (floor(ans) %in% c(0,1,9))){
    return(floor(ans))
  } else if (is.numeric(ans) && !(floor(ans) %in% c(0,1,9))) {
    stop("Invalid request, Must be 0 for No, 1 for Yes or 9 for Unknown")
  } else if (is.character(ans)){
    ans <- switch(ans,
                  "No" = 0,
                  "Yes" = 1,
                  "Unknown" = 9)
    return(ans)
  } else {
    stop("Invalid ans")
  }
}

#' Verify and Convert Manner of Death Code
#'
#' This function takes one parameter, which can be a number or a string. If it is a number,
#' the function verifies it against predefined codes. If it is not one of the codes, it returns an error.
#' If it is a text, the function returns the number corresponding to the code. If it is NULL,
#' the function returns NULL.
#'
#' @inheritParams doris
#'
#' @return The corresponding manner of death code as a number, or an error if the input is invalid.
#' @keywords internal
#'
codify_manner_of_death <- function(manner_of_death) {

  # Define the manner of death codes and their descriptions
  manner_of_death_codes <- c(
    "0" = "Disease",
    "1" = "Accident",
    "2" = "Intentional self-harm",
    "3" = "Assault",
    "4" = "Legal intervention",
    "5" = "War",
    "6" = "Could not be determined",
    "7" = "Pending investigation",
    "9" = "Unknown"
  )

  # If input is NULL, return NULL
  if (is.null(manner_of_death)) {
    return(NULL)
  }

  # If input is a number, check if it is a valid code
  if (is.numeric(manner_of_death)) {
    if (as.character(manner_of_death) %in% names(manner_of_death_codes)) {
      return(manner_of_death)
    } else {
      stop("Invalid manner of death code")
    }
  }

  # If input is a string, find the corresponding code
  if (is.character(manner_of_death)) {
    code <- names(manner_of_death_codes)[match(tolower(manner_of_death), tolower(manner_of_death_codes))]
    if (!is.na(code)) {
      return(as.numeric(code))
    } else {
      stop("Invalid manner of death description")
    }
  }

  # If input is neither a number nor a string, return an error
  stop("Input must be a number, a string or NULL")
}


#' Verify and Convert Maternal Death Time from Pregnancy Code
#'
#' This function takes one parameter, which can be a number or a string. If it is a number,
#' the function verifies it against predefined codes. If it is not one of the codes, it returns an error.
#' If it is a text, the function returns the number corresponding to the code. If it is NULL,
#' the function returns NULL.
#'
#' @param input Either a number or a string representing the maternal death time from pregnancy code.
#'
#' @return The corresponding maternal death time from pregnancy code as a number, or an error
#' if the input is invalid.
#' @export
#'
#' @examples
#' code_maternal_death_time_from_pregnancy(3)          # returns 1
#' code_maternal_death_time_from_pregnancy("Within 42 days before death") # returns 1
#' code_maternal_death_time_from_pregnancy(NULL)        # returns NULL
code_maternal_death_time_from_pregnancy <- function(input) {
  # Define the maternal death time from pregnancy codes and their descriptions
  maternal_death_time_from_pregnancy_codes <- c(
    "0" = "At time of death",
    "1" = "Within 42 days before death",
    "2" = "Between 43 days and 1 year before death",
    "3" = "One year or more before death",
    "9" = "Unknown"
  )

  # If input is NULL, return NULL
  if (is.null(input)) {
    return(NULL)
  }

  # If input is a number, check if it is a valid code
  if (is.numeric(input)) {
    if (as.character(input) %in% names(maternal_death_time_from_pregnancy_codes)) {
      return(input)
    } else {
      stop("Invalid maternal death time from pregnancy code")
    }
  }

  # If input is a string, find the corresponding code
  if (is.character(input)) {
    code <- names(maternal_death_time_from_pregnancy_codes)[match(tolower(input), tolower(maternal_death_time_from_pregnancy_codes))]
    if (!is.na(code)) {
      return(as.numeric(code))
    } else {
      stop("Invalid maternal death time from pregnancy description")
    }
  }

  # If input is neither a number nor a string, return an error
  stop("Input must be a number or a string")
}

#' Verify and Convert Place of Occurrence External Cause Code
#'
#' This function takes one parameter, which can be a number or a string. If it is a number,
#' the function verifies it against predefined codes. If it is not one of the codes, it returns an error.
#' If it is a text, the function returns the number corresponding to the code. If it is NULL,
#' the function returns NULL.
#'
#' @param input Either a number or a string representing the place of occurrence external cause code.
#'
#' @return The corresponding place of occurrence external cause code as a number, or an error if the input is invalid.
#' @export
#'
#' @examples
#' code_place(1)         # returns 1
#' code_place("Residential institution") # returns 1
#' code_place(NULL)       # returns NULL
code_place <- function(input) {
  # Define the place of occurrence external cause codes and their descriptions
  place_of_occurrence_external_cause_codes <- c(
    "0" = "At home",
    "1" = "Residential institution",
    "2" = "School, other institution, public administration area",
    "3" = "Sports and athletics area",
    "4" = "Street and highway",
    "5" = "Trade and service area",
    "6" = "Industrial and construction area",
    "7" = "Farm",
    "8" = "Other place",
    "9" = "Unknown"
  )

  # If input is NULL, return NULL
  if (is.null(input)) {
    return(NULL)
  }

  # If input is a number, check if it is a valid code
  if (is.numeric(input)) {
    if (as.character(input) %in% names(place_of_occurrence_external_cause_codes)) {
      return(input)
    } else {
      stop("Invalid place of occurrence external cause code")
    }
  }

  # If input is a string, find the corresponding code
  if (is.character(input)) {
    code <- names(place_of_occurrence_external_cause_codes)[match(tolower(input), tolower(place_of_occurrence_external_cause_codes))]
    if (!is.na(code)) {
      return(as.numeric(code))
    } else {
      stop("Invalid place of occurrence external cause description")
    }
  }

  # If input is neither a number nor a string, return an error
  stop("Input must be a number or a string")
}


