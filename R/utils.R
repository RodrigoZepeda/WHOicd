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

  req <- request(base_url = url) |>
    req_headers(
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
    res |> req_dry_run()
    response <- NULL
  } else {
    response <- res |> req_perform()
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

