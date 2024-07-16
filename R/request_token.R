#' Request an access token from ICD API
#'
#' @description
#' Obtains an access token for the World Health Organization's (WHO) International
#' Classification of Diseases (ICD) API available at
#' [https://icd.who.int/icdapi](https://icd.who.int/icdapi).
#'
#' @param client_id (`character`) The client ID as provided by the WHO website (see **Generating credentials**).
#' @param client_secret (`character`) The client secret key as provided by the WHO API website (see **Generating credentials**).
#'
#' @section Generating credentials:
#'
#' Go to the World Health Organization API website
#' [https://icd.who.int/icdapi](https://icd.who.int/icdapi)
#' and **Register** in the **API Access** section. Once you have registered and
#' accepted the terms and conditions click on `View API access key(s)`.
#'
#' You will get a `ClientId` and a `ClientSecret`. Those strings are the ones
#' required to use the WHO API.
#'
#' @return Access token for the WHO API. **Don't share this token!!**
#'
#' @import httr2
#' @export
#'
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#' }
get_token <- function(client_id, client_secret) {

  #Get the endpoint for the token generation
  token_endpoint <- "https://icdaccessmanagement.who.int/connect/token"

  token_req <- httr2::request(base_url = token_endpoint) |>
    httr2::req_body_form(
      client_id = client_id,
      client_secret = client_secret,
      scope = "icdapi_access",
      grant_type = "client_credentials"
    )

  # Obtain the token
  token <- token_req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Add a creation time to token
  token["creation_time"] <- Sys.time()
  token["client_id"]     <- client_id #FIXME: Improve the security here
  token["client_secret"] <- client_secret

  return(token)
}

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
check_token_expiration_time <- function(token, auto_update = TRUE) {

  #Calculate the time since creation
  time_since_creation <- difftime(Sys.time(), token$creation_time, units = "secs") |> as.numeric()

  #Check if token will expire
  if (abs(time_since_creation - token$expires_in) < 3 * 60) {
    if (auto_update) {
      token <- get_token(
        client_id     = unlist(token["client_id"]),
        client_secret = unlist(token["client_secret"])
      )
    } else {
      warning("Token will expire in the next 3 minutes. Use `get_token() to generate a new one.")
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
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   res <- make_request(
#'       url = 'https://id.who.int/icd/release/10/E14.1',
#'       token = token,
#'   )
#'   #Use that request with the API:
#'   res |> req_perform()
#'  }
#'
make_request <- function(url, token, language = "en", auto_update = TRUE) {

  # Check whether the token will expire soon or has expired
  token <- check_token_expiration_time(token, auto_update = auto_update)

  req <- httr2::request(base_url = url) |>
    httr2::req_headers(
      Authorization = paste(token["token_type"], token["access_token"]),
      Accept = "application/json",
      `Accept-Language` = language,
      `API-Version` = "v2",
    )
  return(req)
}

#' Create and perform a request to the WHO API v2.
#'
#' This is the main function for development. Creates a request for the API using `make_request`
#' and then performs the request using `httr2::req_perform`. Finally it obtains the body of the
#' response and returns to user.
#'
#' @return The response's body from the request
#'
#' @inheritParams make_request
#'
#' @param post_process_function Function to apply to the body of the response after
#' obtaining a response.
#'
#' @keywords internal
#' @examples
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'
#'   #Get the URL
#'   url <- "https://id.who.int/icd/release/10"
#'
#'   #Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   request_WHO(url, token = token)
#' }
request_WHO <-  function(url,
                         token = NULL,
                         language = "en",
                         auto_update = TRUE,
                         warning_message_404 = "url for request not found",
                         post_process_function = function(x){x}){

  if (curl::has_internet()){

    #Request information from the WHO and obtain the body
    req <- tryCatch(
      {
        # Request the release list to entity
        req <- make_request(
          url = url,
          token = token,
          language = language,
          auto_update = auto_update
        )
        httr2::req_perform(req = req)
      },

      # Return NULL if not found anywhere
      httr2_http_404 = function(cnd) {
        warning(warning_message_404)
        return(NA_character_)
      }
    )

    # Obtain the request of the body
    if (any(!is.na(req))) {
      req <- req |> resp_body_json()
      req <- post_process_function(req)
    }
  } else {
    warning("No Internet connection detected. Returning `NULL`.")
    req <- NA_character_
  }

  return(req)
}
