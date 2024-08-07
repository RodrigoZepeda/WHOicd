# Collection of functions for development of ICD-10 related requests/processing
# exposed to the user.

#' Request releases for ICD-10 from WHO API v2
#'
#' @description Obtains a character vector of all of the ICD-10 releases registered in the API.
#'
#' @inheritParams request_WHO
#'
#' @returns Character vector with the ICD-10 releases available.
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-10 releases
#'   icd10_releases(token)
#' }
#'
#' @export
icd10_releases <- function(token, language = "en", auto_update = TRUE) {

  # Request information from the WHO and obtain the body
  get_release_numbers <- request_WHO("https://id.who.int/icd/release/10",
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = paste(
      "Request not found. Possibly is not",
      "available in the requested language or it was incorrectly specified."
    ),
    post_process_function = function(x) {
      gsub(
        paste0(".*icd/release/10/"),
        "",
        unlist(x["release"], use.names = FALSE)
      )
    }
  )

  return(get_release_numbers)
}

#' Request information on a certain ICD-10 release
#'
#' @description Obtains publishing information available for a specific
#' release of the International Classification of Diseases ICD-10.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @param release (`character`) ICD-10 release (`default = 2019`). Use
#' [icd10_releases()] to get all available releases.
#'
#' @returns vector with information on the publishing date and title
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_release_info(token, release = 2016)
#' }
#'
#' @export
icd10_release_info <- function(token, release = 2019, language = "en", auto_update = TRUE) {

  releases <- request_WHO(
    url = paste0("https://id.who.int/icd/release/10/", release),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = paste(
      "Request not found. Possibly release is not",
      "available for the language requested or incorrectly specified."
    )
  )

  return(releases)
}

#' @title ICD-10 titles
#'
#' @description Obtain the title of an ICD-10 chapter, block or code
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @param search_val (`character`) ICD-10 chapter, block or code for searching.
#' @param as_data_frame Whether to return the result as a `data.frame` or as a vector.
#' @returns A character with the title of the chapter, block or code
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_title(token, "XII")
#'
#'   #Do multiple at once
#'   icd10_title(token, c("XII","IX","XII","II","D50","D50.1"))
#' }
#'
#' @export
icd10_title <- function(token, search_val, release = 2019, language = "en",
                                auto_update = TRUE, as_data_frame = TRUE) {


  to_search <- unique(search_val)
  searched  <- c()
  for (val in to_search){

    # Request the release list to entity
    site_name <- request_WHO(
      url = paste0("https://id.who.int/icd/release/10/", release, "/", toupper(val)),
      token = token,
      language = language,
      auto_update = auto_update,
      warning_message_404 = paste(
        "Request not found. Possibly any of release, chapter/block/code or language is not",
        "available or incorrectly specified."
      ),
      post_process_function = function(site_name) {
        site_name <- unlist(site_name["title"])
        site_name <- as.character(site_name["title.@value"])
        return(site_name)
      }
    )

    searched <- c(searched, site_name)
  }

  if (as_data_frame){
    value_searched <- cbind(to_search, data.frame(title = searched))
    value_searched <- data.frame(searched = search_val) |>
      merge(value_searched, by.x = "searched", by.y = "to_search", all.x = TRUE, sort = FALSE)

  } else {
    value_searched <- searched
  }

  return(value_searched)
}

#' Chapters of ICD-10
#'
#' @description Lists the available chapters for an ICD-10 release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @inheritParams icd10_name_children
#'
#' @returns Data frame with all the chapters and their description
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_chapters(token, release = 2016)
#' }
#'
#' @export
icd10_chapters <- function(token, release = 2019, language = "en", codes_only = FALSE) {
  icd10_name_children(
    token,
    site = "",
    language = language,
    release = release,
    codes_only = codes_only
  )
}


#' Block of ICD-10 Chapter
#'
#' @description Lists the available sections inside a chapter for an ICD-10
#' release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_title
#' @inheritParams icd10_name_children
#' @param chapter Roman numeral indicating the chapter of interest to obtain the blocks from.
#'
#' @returns Data frame with all the blocks inside a chapter and their
#' description
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_blocks(token, chapter = "IX", release = 2016)
#' }
#'
#' @export
icd10_blocks <- function(token, chapter, release = 2019, language = "en", codes_only = FALSE) {
  icd10_name_children(
    token = token,
    site = chapter,
    release = release,
    language = language,
    codes_only = codes_only
  )
}

#' Codes of ICD-10 block
#'
#' @description Lists the available codes inside a block for an ICD-10
#' release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_title
#' @inheritParams icd10_name_children
#'
#' @returns Data frame with all the codes inside a block and their
#' description
#'
#' @param block Character with range of values that conform an ICD-10 code (such as `"I70-I79"`).
#' Note that not all possible ranges are blocks. See [icd10_blocks()] to list all blocks for a
#' chapter.
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_codes(token, "I70-I79")
#' }
#'
#' @export
icd10_codes <- function(token, block, release = 2019, language = "en", codes_only = FALSE) {
  icd10_name_children(
    token = token,
    site = block,
    release = release,
    language = language,
    codes_only = codes_only
  )
}

#' Search for ICD-10 code across releases
#'
#' Searches for a specific ICD-10 code across different releases and returns
#' the editions of those releases in which the code appears.
#' @inheritParams make_request
#' @inheritParams icd10_title
#' @param code ICD-10 code (can be 3 or 4 digit) to search.
#' @param validate_code Check that value seems a valid ICD-10 code before querying the API.
#'
#' @returns A vector of characters with the ICD-10 releases where you can find
#' the ICD-10 code
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_code_search_release(token, "I70")
#'
#' }
#'
#' @export
icd10_code_search_release <- function(token, code, language = "en", validate_code = TRUE,
                                      auto_update = TRUE) {
  # Request the release list to entity
  releases <- request_WHO(
    url = paste0("https://id.who.int/icd/release/10/", code),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = paste(
      "Request not found. Possibly chapter/block/code or language is not",
      "available or incorrectly specified."
    ),
    post_process_function = function(releases) {
      releases <- releases[grepl("release", names(releases))]
      releases <- unlist(releases, use.names = FALSE)
      releases <- sub(".*/([^/]+)/[^/]+$", "\\1", releases)
      return(releases)
    }
  )
  return(releases)
}

#' Search the ICD-10 for information recursively
#'
#' Search the ICD-10 using the WHO API for information on blocks, chapters or codes.
#'
#'
#' @details Searches for either `codes`, `blocks` or `chapters` in a specified `release`.
#' @param search_vals Either `codes`, `blocks` or `chapter` values to search.
#'
#' @inheritParams icd10_blocks
#' @inheritParams make_request
#' @examples
#'
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search multiple titles
#'   dbf <- icd10_search(token, c("D50.1","I14","XII","D50","D60","D50-D53"))
#'
#' }
#' @export
icd10_search <- function(token, search_vals, release = 2019, language = "en", auto_update = TRUE) {

  #Search for the results
  search_results <- .icd10_search_recursive(token = token, search_vals = search_vals,
                                            release = release, language = language,
                                            auto_update = auto_update)

  #Remove last column of empty parents
  search_results <- order_columns(search_results)[,-ncol(search_results)]
  search_vals    <- search_results[,"level_0"]
  search_results <- right_align_df(search_results)
  search_results <- name_df(search_results)

  #Add searched values to column
  search_results <- cbind(searched = search_vals, search_results)

  return(search_results)
}

#' Search the ICD-10 for information recursively
#'
#' Search the ICD-10 using the WHO API for information on blocks, chapters or codes.
#'
#'
#' @details Searches for either `codes`, `blocks` or `chapters` in a specified `release`.
#' @param search_vals Either `codes`, `blocks` or `chapter` values to search.
#'
#' @inheritParams icd10_blocks
#' @examples
#'
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search multiple titles
#'   dbf <- .icd10_search_recursive(token, c("D50.1","I14","XII","D50","D60","D673845"))
#' }
#' @keywords internal
.icd10_search_recursive <- function(token, search_vals, release = 2019, language = "en",
                                    auto_update = TRUE, k = 0) {

  #Get the unique search values not to search them again
  search_values <- unique(search_vals)
  search_values <- search_values[!is.na(search_values)]

  if (length(search_values) > 0){

    titles  <- c()
    parents <- c()
    for (val in search_values){

      #Get title of current disease
      title_val <- icd10_title(
        token = token,
        search_val = val,
        release = release,
        language = language,
        auto_update = auto_update,
        as_data_frame = FALSE)
      titles <- c(titles, title_val)

      #Get parents
      parent_val <- icd10_parent(
        token = token,
        site = val,
        release = release,
        language = language,
        codes_only = FALSE,
        auto_update = auto_update)

      parents <- c(parents, parent_val)

    }

    bind_mat <- as.data.frame(cbind(titles, search_values, parents))

    #Transform searchvals to dataframe and return as binded
    search_values <- data.frame(search_values = search_vals) |>
      merge(bind_mat, by.x = "search_values", by.y = "search_values", all.x = TRUE, sort = FALSE)

    colnames(search_values) <- c(paste0(c("level_", "title_"), k), paste0("level_", k + 1))

    while(length(parents) > 0 & !all(is.na(parents))){
      k <- k + 1
      parent_search <- .icd10_search_recursive(token, unique(parents), release, language, auto_update, k = k)
      parents       <- parent_search[,ncol(parent_search)]
      search_values <- search_values |>
        merge(parent_search, by.x = paste0("level_", k), by.y = paste0("level_", k),
              all.x = TRUE, sort = FALSE)

    }
  }

  return(search_values)
}

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
      titles[k] <- icd10_title(
        token = token,
        search_val = children[k],
        release = release,
        language = language,
        as_data_frame = FALSE
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
