# Collection of functions for development of ICD-10 related requests/processing
# exposed to the user.

#' Request releases for ICD-10 from WHO API v2
#'
#' @description Obtains a character vector of all of the ICD-10 releases registered in the API.
#'
#' @inheritParams request_WHO
#' @inheritParams get_token
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
    ),
    post_process_function = function(releases) {
      releases <- releases[!grepl("child|parent", names(releases))]
      releases <- unlist(releases)
      names(releases) <- gsub("@", "", names(releases))
      return(releases)
    }
  )

  return(releases)
}

#' @title ICD-10 titles
#' @name titles
#'
#' @description Obtain the title of an ICD-10 chapter, block or code
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @param chapter (`character`) ICD-10 chapter as a roman numeral
#' @param block ICD-10 section codes (hyphenated section codes)
#' @param code (`character`) ICD-10 code
#' @param validate_code ( `logical`) Apply additional functions to validate that
#' the ICD-10 code is written as the API needs it.
#'
#' @returns A character with the title of the chapter, block or code
NULL

#' @rdname titles
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_chapter_title(token, "XII")
#' }
#'
#' @export
icd10_chapter_title <- function(token, chapter, release = 2019, language = "en",
                                auto_update = TRUE) {

  # Request the release list to entity
  site_name <- request_WHO(
    url = paste0("https://id.who.int/icd/release/10/", release, "/", toupper(chapter)),
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

  return(site_name)
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


#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_block_title(token, "L50-L54")
#' }
#'
#' @rdname titles
#' @export
icd10_block_title <- function(token, block, release = 2019, language = "en") {
  icd10_chapter_title(
    token = token,
    chapter = block,
    release = release,
    language = language,
  )
}

#' Block of ICD-10 Chapter
#'
#' @description Lists the available sections inside a chapter for an ICD-10
#' release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_chapter_title
#' @inheritParams icd10_name_children
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

#' @inheritParams icd10_block_title
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#'
#' @rdname titles
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd10_code_title(token, "E14.1")
#' }
#'
#' @export
icd10_code_title <- function(token, code, release = 2019, language = "en", validate_code = TRUE) {
  if (validate_code) {
    code <- icd10_validate_code(code)
  }

  # Obtain the code
  icd10_chapter_title(
    token = token,
    chapter = code,
    release = release,
    language = language
  )
}

#' Codes of ICD-10 block
#'
#' @description Lists the available codes inside a block for an ICD-10
#' release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_block_title
#' @inheritParams icd10_name_children
#'
#' @returns Data frame with all the codes inside a block and their
#' description
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
#' @inheritParams icd10_code_title
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
  if (validate_code) {
    code <- icd10_validate_code(code)
  }

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
#'   dbf <- icd10_search(token, c("D50.1","I14","XII","D50","D60","HEY", "D50-D53", NA_character_))
#'   dbf <- icd10_search(token, c("XI","XII", NA_character_))
#'   dbf <- icd10_search(token, c("D50-D53", "D50-D53", NA_character_, "D60-D64"))
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
