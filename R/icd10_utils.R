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
.icd10_request_children <- function(token, site, release = 2019, language = "en",
                                   dry_run = FALSE){

  #Request the list of sections to chapter
  res <- make_request(
    url = paste0('https://id.who.int/icd/release/10/', release,"/", site),
    token = token,
    language = language
  )
  req <- run_request(res = res, dry_run = dry_run)

  #Get the chapter numbers
  if (!is.null(req)){
    releases <- req |> resp_body_json()
    releases <- unlist(releases[grepl("child", names(releases))],
                       use.names = FALSE)
    children <- gsub(paste0(".*release/10/",release,"/"), "", releases)
  } else {
    children <- NULL
  }

  return(children)

}

#' ICD-10 site title
#'
#' @description Obtain the title of an ICD-10 site
#' as requested to `https://id.who.int/icd/release/10/\{release\}/\{site\}`
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @inheritParams icd10_release_info
#' @inheritParams .icd10_request_children
#'
#' @returns A character with the title of the site
#'
#' @export
.icd10_site_title <- function(token, site, release = 2019,
                              language = "en", dry_run = FALSE){

  #Request the release list to entity
  res <- make_request(
    url = paste0('https://id.who.int/icd/release/10/', release,"/",
                 toupper(site)),
    token = token,
    language = language
  )
  req <- run_request(res = res, dry_run = dry_run)

  #Get the chapter numbers
  if (!is.null(req)){
    site_name <- req |> resp_body_json()
    site_name <- unlist(site_name["title"])
    site_name <- as.character(site_name["title.@value"])
  } else {
    site_name <- NULL
  }

  return(site_name)

}

#' ICD-10 chapter, section or code child node names
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`,
#' lists the child nodes and names them.
#'
#' @inheritParams .icd10_request_children
#' @param codes_only Return only the codes without the titles. This option
#' is faster as it realizes less requests.
#'
#' @returns A `data.frame` with the names of all the children (column `title`)
#' available at `site` as well as their codes (column `code`).
#'
#' @keywords internal
.icd10_name_children <- function(token, site, release = 2019, language = "en",
                                   dry_run = FALSE, codes_only = FALSE){

  children <- .icd10_request_children(token = token, site = site,
                                      release = release, language = language,
                                      dry_run = dry_run)

  if (length(children) > 0 & !codes_only){

    #Loop through all chapters and create a data.frame
    titles <- rep(NA_character_, length(children))
    for (k in 1:length(children)){
      titles[k] <- .icd10_site_title(token = token,
                                     site = children[k],
                                     release = release,
                                     language = language,
                                     dry_run = dry_run)
    }

    #Create data.frame
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
.icd10_validate_code <- function(code){

  #Capitalize
  code <- toupper(code)

  #Check that it has a point if the number of characters for ICD-10 code is
  #larger than 3
  if (nchar(code) > 3){
    if (substring(code, 4, 4) != "."){
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
#' @inheritParams .icd10_name_children
#'
#' @returns A character containing the `site` of the parent node.
#'
#' @keywords internal
.icd10_parent <- function(token, site, release = 2019, language = "en",
                            dry_run = FALSE, codes_only = FALSE){


  #Request the release list to entity
  res <- make_request(
    url = paste0('https://id.who.int/icd/release/10/', release,"/",
                 toupper(site)),
    token = token,
    language = language
  )
  req <- run_request(res = res, dry_run = dry_run)

  #Get the parent code
  if (!is.null(req)){
    parent_name <- req |> resp_body_json()
    parent_name <- unlist(parent_name["parent"])
    parent_name <- as.character(parent_name)
    parent_name <- gsub(paste0('.*release/10/', release,"/?"), "", parent_name)
    parent_name <- ifelse(nchar(parent_name) == 0, NA_character_, parent_name)
  } else {
    parent_name <- NA_character_
  }

  return(parent_name)

}



#' ICD-10 parent search
#'
#' @description Generates a request to
#' `https://id.who.int/icd/release/10/\{release\}/\{site\}`,
#' lists the parent nodes and names them (recursively).
#'
#' @inheritParams .icd10_name_children
#'
#' @returns A named vector containing all the parents for the current site
#'
#' @keywords internal
.icd10_parents <- function(token, site, release = 2019, language = "en",
                            dry_run = FALSE, codes_only = FALSE){

  parents <- c()
  site_has_parent <- TRUE
  current_parent  <- site

  while(site_has_parent){

    #Get the parents of the site
    current_parent  <- .icd10_parent(token = token, site = current_parent,
                                      release = release, language = language,
                                      dry_run = dry_run)

    #Indicate whether the parents exist
    site_has_parent <- !is.na(current_parent)

    if (site_has_parent){
      parents         <- c(parents, "code" = current_parent)

      #Obtain name of the parent
      if (!codes_only){
        name_parent <- .icd10_site_title(token = token, site = current_parent,
                                         release = release, language = language,
                                         dry_run = dry_run)
      } else {
        name_parent <- ""
      }

      #Add name of parent to list of names
      parents <- c(parents, "title" =  name_parent)
    }
  }

  if (!dry_run){
    names(parents) <- make.unique(names(parents))
  } else {
    parents <- NULL
  }

  return(parents)
}

#' Search info in a vectorized way
#'
#' @description
#' Searches for chapter, block or code in a vectorized fashion
#'
#' @param searchvec Vector to be searched
#' @param searchfun Search function to be utilized
#' @param ... Additional parameters to pass to `searchfun`
#' @inheritParams .icd10_parents
#'
#' @returns A data frame containing all the parent nodes for the block/code/chapter
#' @keywords internal
.icd10_search_vectorized <- function(searchvec, searchfun, token, release,
                                     language, dry_run, codes_only, ...){

  #Obtain unique entries of the code vector
  uniquevec <- unique(searchvec)

  #Loop through all entries
  res <- lapply(uniquevec, function(x) {
    codeinfo <- searchfun(token, x, release = release, language = language,
                          dry_run = dry_run, codes_only = codes_only,
                          ...)
    if (is.na(codeinfo[1])){
      codeinfo <- NULL
    } else {
      codeinfo <- c(codeinfo, "search_value" = x)
    }
    return(codeinfo)
  })

  # Convert each list to a dataframe and bind them together
  dbf <- dplyr::bind_rows(res)

  #Check that it has rows
  if (nrow(dbf) > 0){
    #Join the dataframe
    dbf <- data.frame(search_value = searchvec) |>
      dplyr::left_join(
        dbf,
        by = dplyr::join_by(search_value)
      ) |>
      dplyr::select(as.symbol("search_value"), dplyr::everything())
  } else {
    warning("No value of `searchvec` was found")
    dbf <- data.frame(search_value = searchvec)
  }

  return(dbf)
}

