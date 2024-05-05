#' Request an access token from ICD API
#'
#' @description
#' Obtains an access token for the World Health Organization's (WHO) International
#' Classification of Diseases (ICD) API available at
#' [https://icd.who.int/icdapi](https://icd.who.int/icdapi).
#'
#' @param client_id (`character`) The client ID as provided by the WHO website (see **Generating credentials**).
#' @param client_secret (`character`) The client secret key as provided by the WHO API website (see **Generating credentials**).
#' @param dry_run (`logical`) If `TRUE` will create the request and generate an empty token. This
#' function is mostly for development.
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
#' #Substitute CLIENT_ID and CLIENT_SECRET for your ID and secret
#' CLIENT_ID     <- Sys.getenv("CLIENT_ID")
#' CLIENT_SECRET <- Sys.getenv("CLIENT_SECRET")
#' token <- get_token(CLIENT_ID, CLIENT_SECRET, dry_run = TRUE)
get_token <- function(client_id, client_secret, dry_run = FALSE){

  token_endpoint <- 'https://icdaccessmanagement.who.int/connect/token'

  token_req <- httr2::request(base_url = token_endpoint) |>
    httr2::req_body_form(
      client_id = client_id,
      client_secret = client_secret,
      scope = 'icdapi_access',
      grant_type = 'client_credentials'
    )

  if (dry_run){
    token_req |> httr2::req_dry_run()
    token <- NULL
  } else {

    #Obtain the token
    token <- token_req |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    #Add a creation time to token
    token["creation_time"] <- Sys.time()
  }

  return(token)
}
