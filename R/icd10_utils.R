# Collection of functions for development of ICD-10 related requests/processing
# not exposed to the user.

#' ICD-10 chapter, section or code child node request
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`
#' and lists the child nodes
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @param site Specific link according to the WHO Swagger API
#' `https://id.who.int/swagger/index.html`
#'
#' @returns A vector with all the children node codes available on `site`.
#'
#' @keywords internal
icd10_request_children <- function(token, site, release = 2019,
                                   language = "en", auto_update = TRUE){

  # Request the release list to entity
  children <- request_WHO(
    url = paste0("https://id.who.int/icd/release/10/", release, "/", site),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = paste(
      "Request not found. Possibly any of release, chapter/block/code or language is not",
      "available or incorrectly specified."
    ),
    post_process_function = function(releases){
      releases <- unlist(releases[grepl("child", names(releases))], use.names = FALSE)
      releases <- gsub(paste0(".*release/10/", release, "/"), "", releases)
      return(releases)
    }
  )

  return(children)
}

#' ICD-10 chapter, section or code child node names
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`,
#' lists the child nodes and names them.
#'
#' @inheritParams icd10_request_children
#' @param codes_only Return only the codes without the titles. This option
#' is faster as it realizes less requests.
#'
#' @returns A `data.frame` with the names of all the children (column `title`)
#' available at `site` as well as their codes (column `code`).
#'
#' @keywords internal
icd10_name_children <- function(token, site, release = 2019, language = "en", codes_only = FALSE) {

  children <- icd10_request_children(
    token = token,
    site = site,
    release = release,
    language = language
  )

  if (length(children) > 0 & !codes_only) {
    # Loop through all chapters/blocks/codes and create a data.frame
    titles <- rep(NA_character_, length(children))
    for (k in 1:length(children)) {
      titles[k] <- icd10_chapter_title(
        token = token,
        chapter = children[k],
        release = release,
        language = language
      )
    }

    # Create data.frame
    children_df <- data.frame(`codes` = children, `title` = titles)
  } else if (codes_only) {
    children_df <- data.frame(`codes` = children)
  } else {
    children_df <- NULL
  }

  return(children_df)
}

#' ICD-10 code validation
#'
#' Takes an ICD-10 code and transforms it using capital letters. It also
#' checks that it has a point for the last digits (so e141 is transformed to
#' E14.1)
#'
#' @param code (`character`) ICD-10 code
#'
#' @returns A character that is capitalized and has a dot separating the
#' first 3 digits with the next digits so that it might resemble an ICD-10
#' code
#'
#' @keywords internal
icd10_validate_code <- function(code) {
  # Capitalize
  code <- toupper(code)

  # Check that it has a point if the number of characters for ICD-10 code is
  # larger than 3
  if (nchar(code) > 3) {
    if (substring(code, 4, 4) != ".") {
      code <- paste0(substring(code, 1, 3), ".", substring(code, 4))
    }
  }

  return(code)
}

#' ICD-10 parent search
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`,
#' and obtains the parent node.
#'
#' @inheritParams icd10_name_children
#'
#' @returns A character containing the `site` of the parent node.
#'
#' @keywords internal
icd10_parent <- function(token, site, release = 2019, language = "en", codes_only = FALSE,
                          auto_update = TRUE) {

  # Request the release list to entity
  res <- request_WHO(
    url = paste0("https://id.who.int/icd/release/10/", release, "/", toupper(site)),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = paste(
      "Request not found. Possibly any of release, chapter/block/code or language is not",
      "available or incorrectly specified."
    ),
    post_process_function = function(parent_name){
      parent_name <- unlist(parent_name["parent"])
      parent_name <- as.character(parent_name)
      parent_name <- gsub(paste0(".*release/10/", release, "/?"), "", parent_name)
      if (nchar(parent_name) != 0){
        return(parent_name)
      } else {
        return(NA_character_)
      }
    }
  )

  return(res)
}

#' ICD-10 parent search
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`,
#' lists the parent nodes and names them (recursively).
#'
#' @inheritParams icd10_name_children
#'
#' @returns A named vector containing all the parents for the current site
#'
#' @keywords internal
icd10_parents <- function(token, site, release = 2019, language = "en", codes_only = FALSE) {

  #Set the parents
  parents         <- c()
  site_has_parent <- TRUE
  current_parent  <- site

  while (site_has_parent) {
    # Get the parents of the site
    current_parent <- icd10_parent(
      token = token,
      site = current_parent,
      release = release,
      language = language
    )

    # Indicate whether the parents exist
    site_has_parent <- !is.na(current_parent) & !is.null(current_parent)

    if (site_has_parent) {
      parents <- c(parents, "code" = current_parent)

      # Obtain name of the parent
      if (!codes_only) {
        name_parent <- icd10_chapter_title(
          token = token,
          chapter = current_parent,
          release = release,
          language = language
        )
      } else {
        name_parent <- ""
      }

      # Add name of parent to list of names
      parents <- c(parents, "title" = name_parent)

      #Get names of the parents
      names(parents) <- make.unique(names(parents))
    }
  }

  return(parents)
}
