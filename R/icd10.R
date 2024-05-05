# Collection of functions for development of ICD-10 related requests/processing
# exposed to the user.

#' Request releases for ICD-10
#'
#' @description Obtains a vector of all of the ICD-10 releases.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#'
#' @returns Vector with the ICD-10 releases available.
#'
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#'
#' #Search for ICD-10 releases
#' icd10_releases(token, dry_run = TRUE)
#' @export
icd10_releases <- function(token, language = "en", dry_run = FALSE){

  #Request the release list to entity
  res <- make_request(
    url = 'https://id.who.int/icd/release/10',
    token = token,
    language = language
  )
  req <- run_request(res = res, dry_run = dry_run)

  if (!is.null(req)){
    releases            <- req |> resp_body_json()
    get_release_numbers <-  gsub(paste0(".*icd/release/10/"), "",
                                 unlist(releases["release"], use.names = FALSE)
                                 )
  } else {
    get_release_numbers <- NULL
  }

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
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#'
#' #Get title and url for release
#' icd10_release_info(token, release = 2016, dry_run = TRUE)
#' @export
icd10_release_info <- function(token, release = 2019, language = "en",
                               dry_run = FALSE){

  #Request the release list to entity
  res <- make_request(
    url = paste0('https://id.who.int/icd/release/10/', release),
    token = token,
    language = language
  )
  req <- run_request(res = res, dry_run = dry_run)

  if (!is.null(req)){
    releases <- req |> resp_body_json()
    releases <- releases[!grepl("child|parent", names(releases))]
    releases <- unlist(releases)
    names(releases) <- gsub("@", "", names(releases))
  } else {
    releases <- NULL
  }

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
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_chapter_title(token, "XII", dry_run = TRUE)
#'
#' @export
icd10_chapter_title <- function(token, chapter, release = 2019,
                                language = "en", dry_run = FALSE){

  .icd10_site_title(token = token, site = chapter,
                    release = release, language = language, dry_run = dry_run)

}

#' Chapters of ICD-10
#'
#' @description Lists the available chapters for an ICD-10 release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @inheritParams .icd10_name_children
#'
#' @returns Data frame with all the chapters and their description
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_chapters(token, release = 2016, dry_run = TRUE)
#'
#' @export
icd10_chapters <- function(token, release = 2019, language = "en",
                           dry_run = FALSE, codes_only = FALSE){

  .icd10_name_children(token, site = "", language = language,
                       release = release, dry_run = dry_run,
                       codes_only = codes_only)


}


#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#'
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_block_title(token, "L50-L54", dry_run = TRUE)
#'
#' @rdname titles
#' @export
icd10_block_title <- function(token, block, release = 2019,
                              language = "en", dry_run = FALSE){

  .icd10_site_title(token = token, site = block,
                    release = release, language = language, dry_run = dry_run)

}

#' Block of ICD-10 Chapter
#'
#' @description Lists the available sections inside a chapter for an ICD-10
#' release.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_chapter_title
#' @inheritParams .icd10_name_children
#'
#' @returns Data frame with all the blocks inside a chapter and their
#' description
#'
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_blocks(token, chapter = "IX", release = 2016, dry_run = TRUE)
#' @export
icd10_blocks <- function(token, chapter, release = 2019, language = "en",
                        dry_run = FALSE, codes_only = FALSE){

  .icd10_name_children(token = token, site = chapter, release = release,
                       language = language, dry_run = dry_run,
                       codes_only = codes_only)

}

#' @inheritParams icd10_block_title
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#'
#' @rdname titles
#'
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_code_title(token, "E14.1", dry_run = TRUE)
#'
#' @export
icd10_code_title <- function(token, code, release = 2019, language = "en",
                             dry_run = FALSE, validate_code = TRUE){

  if (validate_code){
    code <- .icd10_validate_code(code)
  }

  tryCatch({
    #Search for the code
    .icd10_site_title(token = token, site = code,
                      release = release, language = language,
                      dry_run = dry_run)
    },

    #Return NULL if not found anywhere
    httr2_http_404 = function(cnd) {warning("ICD-10 code possibly not found"); return(NA_character_)}
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
#' @inheritParams .icd10_name_children
#'
#' @returns Data frame with all the codes inside a block and their
#' description
#'
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_codes(token, "I70-I79", dry_run = TRUE)
#'
#' @export
icd10_codes <- function(token, block, release = 2019, language = "en",
                         dry_run = FALSE, codes_only = FALSE){

  .icd10_name_children(token = token, site = block, release = release,
                       language = language, dry_run = dry_run,
                       codes_only = codes_only)

}

#' Search for ICD-10 code across releases
#'
#' Searches for a specific ICD-10 code across different releases and returns
#' the editions of those releases in which the code appears.
#'
#' @inheritParams icd10_code_title
#'
#' @returns A vector of characters with the ICD-10 releases where you can find
#' the ICD-10 code
#'
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_code_search_release(token, "I70", dry_run = TRUE)
#'
#' #Invalid ICD-10 codes should return NULL
#' icd10_code_search_release(token, "NOT-A-CODE")
#' @export
icd10_code_search_release <- function(token, code, language = "en",
                                      dry_run = FALSE, validate_code = TRUE){

  if (validate_code){
    code <- .icd10_validate_code(code)
  }

  #Request the release list to entity
  res <- make_request(
    url = paste0('https://id.who.int/icd/release/10/', code),
    token = token,
    language = language
  )

  releases <- tryCatch({
    req <- run_request(res = res, dry_run = dry_run)

    if (!is.null(req)){
      releases <- req |> resp_body_json()
      releases <- releases[grepl("release", names(releases))]
      releases <- unlist(releases, use.names = FALSE)
      releases <- sub(".*/([^/]+)/[^/]+$", "\\1", releases)
    } else {
      releases <- NULL
    }

    releases
    },

    #Return NULL if not found anywhere
    httr2_http_404 = function(cnd) NULL
  )

  return(releases)
}

#' @title ICD-10 complete information on code, block or chapter
#' @name icd10info
#'
#' @description Obtains the title of an ICD-10 block or code, as well
#' as the chapter (and block) to which it belongs (in a certain release).
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_codes
#' @inheritParams icd10_blocks
#' @inheritParams .icd10_parents
#' @inheritParams icd10_code_title
#'
#' @returns A character with the title of the chapter, block (and code if
#' code is specified).
NULL

#' @rdname icd10info
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_block_info(token, "D60-D64", dry_run = TRUE)
#'
#' @export
icd10_block_info <- function(token, block, release = 2019, language = "en",
                             dry_run = FALSE, codes_only = FALSE){

  #Get block title
  if (!codes_only){
    block_title <- icd10_block_title(token = token, block = block,
                                     release = release, language = language,
                                     dry_run = dry_run)
  } else {
    block_title <- ""
  }

  parent_info <- .icd10_parents(token = token, site = block, release = release,
                                language = language, dry_run = dry_run,
                                codes_only = codes_only)


  #Add block info
  if (!dry_run){
    block_info <- c("block" = block, "block_title" = block_title)
    names(parent_info) <- c("chapter", "chapter_title")
    all_info <- c(block_info, parent_info)
  } else {
    all_info <- NULL
  }

  return(all_info)
}

#' @rdname icd10info
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_code_info(token, "D60", dry_run = TRUE)
#' icd10_code_info(token, "D60.1", dry_run = TRUE)
#'
#' @export
icd10_code_info <- function(token, code, release = 2019, language = "en",
                            dry_run = FALSE, codes_only = FALSE,
                            validate_code = TRUE){

  #Validate code
  if (validate_code){
    code <- .icd10_validate_code(code)
  }

  #Get block title
  if (!codes_only){
    code_title <- icd10_code_title(token = token, code = code,
                                   release = release, language = language,
                                   dry_run = dry_run, validate_code = FALSE)
  } else {
    code_title <- ""
  }

  if (is.na(code_title)){
    return(NA)
  } else {
    #Get parents
    parent_info <- .icd10_parents(token = token, site = code, release = release,
                                  language = language, dry_run = dry_run,
                                  codes_only = codes_only)

    #Add block info
    if (nchar(code) == 3 & !dry_run){

      code_info          <- c("code" = code, "code_title" = code_title)
      names(parent_info) <- c("block","block_title","chapter", "chapter_title")
      all_info           <- c(code_info, parent_info)

    } else if (!dry_run){
      code_info          <- c("subcode" = code, "subcode_title" = code_title)
      names(parent_info) <- c("code","code_title","block","block_title",
                              "chapter", "chapter_title")
      all_info <- c(code_info, parent_info)
    } else {
      all_info <- NULL
    }

    return(all_info)
  }
}

#' @rdname icd10info
#' @examples
#' #Change `dry_run = FALSE` to run the example
#' token <- get_token("123","123", dry_run = TRUE)
#' icd10_chapter_info(token, "II", dry_run = TRUE)
#'
#' @export
icd10_chapter_info <- function(token, chapter, release = 2019, language = "en",
                               dry_run = FALSE, codes_only = FALSE){

  if (!codes_only){
    chapter_title <- icd10_chapter_title(token = token, chapter = chapter,
                                         release = release, language = language,
                                         dry_run = dry_run)

  } else {
    chapter_title <- ""
  }

  all_info <- c("chapter" = chapter, "chapter_title" = chapter_title)
  return(all_info)
}
