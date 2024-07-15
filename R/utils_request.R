#' Check token expiration time
#'
#' Checks whether the token will expire soon or has already expired and
#' throws a warning to the user.
#'
#' @inheritParams make_request
#' @param auto_update Attempts to update token automatically
#'
#' @return NULL It checks whether the token has expired or is soon to expire
#' and alerts the user.
#'
#' @keywords internal
#' @examples
#' check_token_expiration_time(token)
check_token_expiration_time <- function(token = NULL, auto_update = TRUE, dry_run = FALSE) {
  if (!is.null(token)) {
    time_since_creation <- difftime(Sys.time(), token$creation_time,
                                    units = "secs") |> as.numeric()

    if (time_since_creation >= token$expires_in) {
      if (auto_update) {
        message("Auto-updating your token...")
        token <- get_token(
          client_id = unlist(token["client_id"]),
          client_secret = unlist(token["client_secret"]),
          dry_run = dry_run
        )
        message("Token updated!")
      } else {
        stop("Token expired. Use `get_token()` to generate a new one.")
      }
    } else if (abs(time_since_creation - token$expires_in) < 3 * 60) {
      if (auto_update) {
        token <- get_token(
          client_id = unlist(token["client_id"]),
          client_secret = unlist(token["client_secret"]),
          dry_run = dry_run
        )
      } else {
        warning(
          paste0(
            "Token will expire in the next 3 minutes. Use `get_token() ",
            "to generate a new one."
          )
        )
      }
    }
  } else {
    if (!dry_run){
      warning("`NULL` token provided. Please use `get_token()` to generate a token.")
    }
  }
  return(token)
}

#' Make a request for the WHO API v2
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
#' @inheritParams check_token_expiration_time
#'
#' @return An `httr2` request for the API
#' @keywords internal
#' @examples
#' # Make a request
#' res <- make_request(
#'     url = 'https://id.who.int/icd/release/10/E14.1',
#'     token = token,
#' )
#' #Use that request with the API:
#' res |> req_perform()
#'
make_request <- function(url, token, language = "en",
                         auto_update = TRUE, dry_run = FALSE) {

  # Check whether the token will expire soon or has expired
  token <- check_token_expiration_time(
    token,
    auto_update = auto_update,
    dry_run = dry_run
  )

  req <- httr2::request(base_url = url) |>
    httr2::req_headers(
      Authorization = paste(token["token_type"], token["access_token"]),
      Accept = "application/json",
      `Accept-Language` = language,
      `API-Version` = "v2",
    )
  return(req)
}

#' Perform a request for the WHO API
#'
#' @description Performs a request `res` if `dry_run` is not `FALSE`. Returns
#' `NULL` otherwise.
#'
#' @param res An `httr2` request to perform
#' @inheritParams make_request
#'
#' @return Either the result from the request or `NULL` if `dry_run` is `TRUE`
#'
#' @keywords internal
#' @examples
#' #Create the request and perform it
#' res <- make_request(
#'     url = 'https://id.who.int/icd/release/10/E14.1',
#'     token = token,
#' )
#' #Use that request with the API:
#' res |> run_request()
run_request <- function(res, dry_run = FALSE) {
  if (dry_run) {
    res |> httr2::req_dry_run()
    response <- NULL
  } else {
    response <- res |> httr2::req_perform()
  }
  return(response)
}

#' Create and perform a request to the WHO API v2.
#'
#' This is the main function for development. Creates a request for the API using `make_request`
#' and then performs the request using `run_request`. Finally it obtains the body of the
#' response and returns to user.
#'
#' @return Either the response from the request or `NULL` if `dry_run` is `TRUE`
#'
#' @inheritParams make_request
#' @keywords internal
#' @example
#'
#' #Get the URL
#' url <- "https://id.who.int/icd/release/10"
#'
#' #Obtain your token using your id and secret
#' token <- get_token("123", "123", dry_run = TRUE)
#'
#' #Run the request
#' request_WHO("https://id.who.int/icd/release/10", token, dry_run = TRUE)
request_WHO <-  function(url, token = NULL, language = "en", auto_update = TRUE, dry_run = FALSE,
                         warning_message_404 = "url for request not found"){

  #FIXME: Check the combination of has internet with dry run
  if (curl::has_internet()){

    #Request information from the WHO and obtain the body
    req <- tryCatch(
      {
        # Request the release list to entity
        res <- make_request(
          url = url,
          token = token,
          language = language,
          auto_update = auto_update,
          dry_run = dry_run
        )
        run_request(res = res, dry_run = dry_run)
      },

      # Return NULL if not found anywhere
      httr2_http_404 = function(cnd) {
        warning(warning_message_404)
        return(NULL)
      }
    )

    # Obtain the request of the body
    if (!is.null(req)) {
      response_body <- req |> resp_body_json()
    } else {
      response_body <- NULL
    }
  } else {
    warning("No Internet connection detected. Returning NA")
    response_body <- NULL
  }

  return(response_body)
}
