#' Make a request for the WHO API
#'
#' @description
#' Builds a request to the `url` using the `token` obtained
#' from the [get_token()] function.
#'
#' @param url (`character`) Address for the request
#' @param token (`list`) Access token to the API obtained from [get_token()]
#' @param language (`character`) Language for the request. Available languages are
#' listed in [https://icd.who.int/docs/icd-api/SupportedClassifications/](https://icd.who.int/docs/icd-api/SupportedClassifications/)
#' the default is English (`language = "en"`).
#' @return An `httr2` request for the API
#' @keywords internal
make_request <- function(url, token, language = "en"){

  #Check whether the token will expire soon or has expired
  check_token_expiration_time(token)

  req <- httr2::request(base_url = url) |>
    httr2::req_headers(
      Authorization = paste(token["token_type"], token["access_token"]),
      Accept = "application/json",
      `Accept-Language` = language,
      `API-Version` = "v2",
    )
  return(req)
}

#' Run a request for the WHO API
#'
#' @description Runs a request `res` if `dry_run` is not `FALSE`. Returns
#' `NULL` otherwise.
#'
#' @param res An `httr2` request to perform
#' @inheritParams make_request
#'
#' @return Either the result from the request or `NULL` if `dry_run` is `TRUE`
#'
#' @keywords internal
run_request <- function(res, dry_run){
  if (dry_run){
    res |> httr2::req_dry_run()
    response <- NULL
  } else {
    response <- res |> httr2::req_perform()
  }
  return(response)
}

#' Check token expiration time
#'
#' Checks whether the token will expire soon or has already expired and
#' throws a warning to the user.
#'
#' @inheritParams make_request
#'
#' @return NULL It checks whether the token has expired or is soon to expire
#' and alerts the user.
#'
#' @keywords internal
check_token_expiration_time <- function(token){

  if (!is.null(token)){
    time_since_creation <- difftime(Sys.time(), token$creation_time,
                                    units = "secs") |> as.numeric()

    if (time_since_creation >= token$expires_in){
      stop("Token expired. Use `get_token()` to generate a new one.")
    } else if (abs(time_since_creation - token$expires_in) < 3*60){
      warning(
        "Token will expire in the next 3 minutes. Use `get_token() to generate a new one."
      )
    }
  }
}

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
check_names <- function(all_info){

  k <- 1
  while (k <= length(all_info)){
    #If it is a code check whether it is a code, block or chapter
    if (grepl("code", names(all_info[k]))){
      if (grepl("-", all_info[k])){
        names(all_info)[k] <- "block"
        names(all_info)[k+1] <- "block_title"
        k <- k + 1
      } else if (grepl("[0-9]", all_info[k])){
        names(all_info)[k] <- "code"
        names(all_info)[k+1] <- "code_title"
        k <- k + 1
      } else {
        names(all_info)[k] <- "chapter"
        names(all_info)[k+1] <- "chapter_title"
        k <- k + 1
      }
    }
    k <- k + 1
  }

  #Change the numbering of the make-unique
  all_info <- rev(all_info)
  names(all_info) <- make.unique(names(all_info), sep = "_")
  all_info <- rev(all_info)

  return(all_info)
}
