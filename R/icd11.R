#' Request information on ICD-11 release
#'
#' @description Obtains publishing information on the International Classification of Diseases
#' ICD-11.
#'
#' @inheritParams make_request
#' @inheritParams get_token
#' @param release (`character`) ICD-11 release (`default = 2024-01`). See all releases
#' in the WHO's website [https://icd.who.int/docs/icd-api/SupportedClassifications/](https://icd.who.int/docs/icd-api/SupportedClassifications/)
#'
#' @returns list with information on the publishing date and title
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get title and url for release
#'   icd11_release_info(token, release = "2023-01")
#' }
#'
#' @export
icd11_release_info <- function(token, release = "2024-01", language = "en", auto_update = TRUE) {
  releases <- request_WHO(
    url = paste0("https://id.who.int/icd/entity/"),
    token = token,
    language = language,
    auto_update = auto_update,
    req_body = list(releaseId = release),
    warning_message_404 = paste(
      "Request not found. Possibly release is not",
      "available for the language requested or incorrectly specified."
    )
  )

  return(releases)
}

#' Request information on ICD-11 linearization
#'
#' @description Obtains publishing information on the International Classification of Diseases
#' ICD-11's linearization as the Mortality and Morbidity Statistics (MMS) or the
#' International Classifications of Functioning Disability and Health (ICF) . The default is `mms`
#'
#' @inheritParams icd11_release_info
#' @param linearization (`character`) ICD-11 linearization (`default = "mms"`). See all linearizations
#' in the WHO's API website [https://icd.who.int/docs/icd-api/APIDoc-Version2/#the-icd11-linearizations](https://icd.who.int/docs/icd-api/APIDoc-Version2/#the-icd11-linearizations)
#'
#' @returns list with information on the publishing date and title
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Get linearizations for Mortality and Morbidity Statistics
#'   icd11_linearization_info(token, release = "2023-01")
#'
#'   # Get linearizations for International Classifications of Functioning Disability and Health
#'   icd11_linearization_info(token, release = "2023-01", linearization = "icf")
#' }
#'
#' @export
icd11_linearization_info <- function(token, linearization = "mms", release = "2024-01", language = "en", auto_update = TRUE) {
  releases <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", linearization),
    token = token,
    language = language,
    auto_update = auto_update,
    req_body = list(releaseId = release),
    warning_message_404 = paste(
      "Request not found. Possibly release/linearization is not",
      "available for the language requested or incorrectly specified."
    )
  )

  return(releases)
}

#' Autocode
#'
#' Provides the best matching classification entity (its code and URI) for the
#' provided diagnostic text.
#'
#' @param text (stirng) String to autocode with ICD-11
#' @inheritParams doris
#' @param subtrees_filter (string) Optional parameter. Vector of URIs. If provided,
#' the search will be performed on the entities provided and their descendants
#' @param match_threshold (string) Score is a value between 0 and 1 that indicates the similarity
#' between the input text and the matched term. `match_threshold` is the minimum score to be
#' included in the output. Will use WHO's default value if no threshold provided
#' @inheritParams icd11_linearization_info
#'
#' @details The `icd11_autocode` function allows you to search across different linearizations such
#' as the Mortality and Morbidity Statistics `linearization = "mms"` (default) and the
#' International Classifications of Functioning Disability and Health `linearization = "icf"`.
#'
#' @note Both `icd11_autocode()` and `icd11_autocode2()` provide the best classification. They
#' correspond to two different API entry-points in the WHO's API and provide slighly different
#' amount of information for the same result.
#'
#' @returns A list of the coded information as well as any additional information.
#'
#' @name autocode
NULL

#' @rdname autocode
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-10 releases
#'   icd11_autocode(token, text = "Cerebrovascular accident")
#' }
#'
#' @note This provides the default autocode but might throw different amount of information
#' as `icd11_autocode2`.
#'
#' @export
icd11_autocode <- function(token, text, release = "2024-01", linearization = "mms",
                           subtrees_filter = NULL, match_threshold = NULL, language = "en",
                           auto_update = TRUE) {
  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", release, "/", linearization, "/autocode"),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release or linearization for request not found",
    req_body = list(
      searchText = text,
      releaseId = release,
      subtreesFilter = paste0(subtrees_filter, collapse = ", "),
      matchThreshold = match_threshold
    )
  )

  return(autocode)
}

#' @rdname autocode
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-10 releases
#'   icd11_autocode2(token, text = "Cerebrovascular accident")
#' }
#'
#' @export
icd11_autocode2 <- function(token, text, release = "2024-01", subtrees_filter = NULL,
                            match_threshold = NULL, language = "en", auto_update = TRUE) {
  autocode <- request_WHO(
    url = "https://id.who.int/icd/entity/autocode",
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request not found",
    req_body = list(
      searchText = text,
      releaseId = release,
      subtreesFilter = paste0(subtrees_filter, collapse = ", "),
      matchThreshold = match_threshold
    )
  )

  return(autocode)
}

#' Search
#'
#' Searches the foundation component of the ICD-11.
#'
#' @param text The text component to search in ICD-11.
#' @inheritParams autocode
#' @inheritParams doris
#'
#' @param subtrees_filter_foundation_descendents When a `subtrees_filter` is used the search looks at
#' the linearization descendants of the entities in the `subtrees_filter`. If
#'
#' `subtrees_filter_foundation_descendents` is set to `TRUE`, the search looks at the
#' foundation descendants of the entities in the `subtrees_filter`. Default value: `FALSE`
#'
#' @param include_keyword_result Boolean. Optional parameter. If set to `TRUE`, the search result
#' will also include keyword list. Default value: `FALSE`
#'
#' @param chapter_filter Vector of chapter codes e.g., c("01","02","21").
#' When provided, the search will be performed only on these chapters.
#'
#' @param use_flexisearch Boolean. Changes the search mode to flexible search. Default value: `FALSE`
#'
#' @param flat_results Boolean. Optional parameter. If set to true the search result entities are
#' provided in a nested data structure representing the ICD-11 hierarchy. Default value: true.
#'
#' @param highlighting_enabled Boolean. Optional. If set to `FALSE` the search result highlighting
#' is turned off. Default value: `FALSE`. Highlighting is in html format.
#'
#' @param medical_coding_mode Boolean. This mode is the default and should be used when searching
#' the classification for medical coding purposes. Default value: `TRUE`.
#'
#' @param properties_to_be_searched String. The properties to be included in the search.
#' Used only when medical_coding_mode=`FALSE`
#'
#'
#' @details The `icd11_search` function allows you to search across different linearizations such
#' as the Mortality and Morbidity Statistics `linearization = "mms"` (default) and the
#' International Classifications of Functioning Disability and Health `linearization = "icf"`.
#'
#' @note Both `icd11_search()` and `icd11_search2()` provide options for search. They
#' correspond to two different API entry-points in the WHO's API and provide slighly different
#' amount of information for the same result.
#'
#' @returns A list of the coded information as well as any additional information.
#'
#' @name search
NULL

#' @rdname search
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-10 releases
#'   icd11_search(token, text = "HIV stage 4")
#' }
#'
#' @note This provides the default search but might throw different amount of information
#' as `icd11_search2`.
#'
#' @export
icd11_search <- function(token,
                         text,
                         release = "2024-01",
                         linearization = "mms",
                         subtrees_filter = NULL,
                         subtrees_filter_foundation_descendents = FALSE,
                         include_keyword_result = FALSE,
                         chapter_filter = NULL,
                         use_flexisearch = FALSE,
                         flat_results = NULL,
                         highlighting_enabled = FALSE,
                         medical_coding_mode = TRUE,
                         properties_to_be_searched = NULL,
                         language = "en",
                         auto_update = TRUE) {

  search <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", release, "/", linearization, "/search"),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release or linearization for request not found",
    method = "POST",
    req_body = list(
      q = text,
      subtreesFilter = paste0(subtrees_filter, collapse = ", "),
      subtreeFilterUsesFoundationDescendants = ifelse(subtrees_filter_foundation_descendents, "true", "false"),
      includeKeywordResult = ifelse(include_keyword_result, "true", "false"),
      chapterFilter = chapter_filter,
      useFlexisearch = ifelse(use_flexisearch, "true", "false"),
      flatResults = flat_results,
      highlightingEnabled = ifelse(highlighting_enabled, "true", "false"),
      medicalCodingMode = ifelse(medical_coding_mode, "true", "false"),
      propertiesToBeSearched = properties_to_be_searched
    )
  )

  return(search)
}

#' @rdname search
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-10 releases
#'   icd11_search2(token, text = "Cerebrovascular accident")
#' }
#'
#' @export
icd11_search2 <- function(token, text,
                          release = "2024-01",
                          linearization = "mms",
                          subtrees_filter = NULL,
                          chapter_filter = NULL,
                          use_flexisearch = FALSE,
                          flat_results = NULL,
                          highlighting_enabled = FALSE,
                          properties_to_be_searched = NULL,
                          language = "en",
                          auto_update = TRUE) {

  secret <- request_WHO(
    url = "https://id.who.int/icd/entity/search",
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request not found",
    method = "POST",
    req_body = list(
      q = text,
      subtreesFilter = paste0(subtrees_filter, collapse = ", "),
      chapterFilter = chapter_filter,
      useFlexisearch = ifelse(use_flexisearch, "true", "false"),
      flatResults = flat_results,
      highlightingEnabled = ifelse(highlighting_enabled, "true", "false"),
      propertiesToBeSearched = properties_to_be_searched,
      releaseId = release
    )
  )

  return(secret)
}

#' Identify an URI in ICD-11
#'
#' Provides all of the characteristics from an URI
#'
#' @param id (stirng) String either with a complete URI of the last digits of the URI
#' @param include Either NULL of provide additional information such as `ancestor`,
#' `decendant` or `diagnosticCriteria`. For example, if you set `include="ancestor"` the returned
#' response will include all ancestor entity URIs for the requested entity. Use a vector to include
#' multiple properties. For example `include = c("ancestor", "diagnosticCriteria")`
#' @inheritParams icd11_autocode
#'
#' @returns Character vector with the ICD-11 URI and its characteristics
#'
#' @details `icd_id()` Searches for a specific URI across a specific `linearization` an `release`.
#' `icd_id2()`  searches for a URI across a specific release regardless of linearization. Finally,
#' `icd_id3` searches for the URI across all ICD-11 releases.
#'
#' @name id
#'
NULL

#' @rdname id
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI
#'   icd11_id(token, id = "http://id.who.int/icd/entity/1442995018")
#'
#'   # Search for ICD-11 URI with parents and diagnostic criteria
#'   icd11_id(token,
#'     id = "http://id.who.int/icd/entity/1442995018",
#'     include = c("ancestor", "diagnosticCriteria")
#'   )
#' }
#' @export
icd11_id <- function(token, id, linearization = "mms", release = "2024-01", include = NULL, language = "en",
                      auto_update = TRUE) {

  # Preprocess ID if URI is provided to keep only last digits
  parts <- strsplit(id, "/")[[1]]
  id    <- utils::tail(parts, n = 1)

  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", release, "/", linearization, "/", id),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request not found",
    req_body = list(
      include = paste0(include, collapse = ", ")
    )
  )

  return(autocode)
}


#' @rdname id
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across all linearizations in a specific release
#'   icd11_id2(token, id = "http://id.who.int/icd/entity/1442995018")
#'
#'   # Search for ICD-11 URI with parents and diagnostic criteria across all linearizations
#'   #  in a specific release
#'   icd11_id2(token,
#'     id = "http://id.who.int/icd/entity/1442995018",
#'     include = c("ancestor", "diagnosticCriteria")
#'   )
#' }
#' @export
icd11_id2 <- function(token, id, release = "2024-01", include = NULL, language = "en",
                      auto_update = TRUE) {

  # Preprocess ID if URI is provided to keep only last digits
  parts <- strsplit(id, "/")[[1]]
  id    <- utils::tail(parts, n = 1)

  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/entity/", id),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request not found",
    req_body = list(
      releaseId = release,
      include = paste0(include, collapse = ", ")
    )
  )

  return(autocode)
}

#' @rdname id
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   icd11_id3(token, id = "http://id.who.int/icd/entity/1442995018")
#'
#' }
#'
#' @export
icd11_id3 <- function(token, id, linearization = "mms", language = "en", auto_update = TRUE) {

  parts <- strsplit(id, "/")[[1]]
  id    <- utils::tail(parts, n = 1)

  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/",linearization,"/", id),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request not found"
  )

  return(autocode)
}

#' Code info
#'
#' Returns information on a certain code.
#'
#' @param code The code or code combination that will be looked up. (The & and / characters need
#' to be URL encoded)
#'
#' @param flexible_mode Normally, `icd11_codeinfo` returns an error when the provided code combination
#' includes codes that are not in the predefined suggested value sets of the entity. If the
#' `flexible_mode` is set to `TRUE`, it will return a result even when some of the postcoordination
#' values are not in these defined value sets. In this case, the codes whose postcoordination axis
#' cannot be detected ('/' postcoordination) will be placed under otherPostcoordination rubric.
#'
#' @param convert_2_terminal by default it is `FALSE`, if set to `TRUE`, the codes will be changed
#' to use the terminal codes (leaf nodes in the hierarchy). In most cases this is done by changing
#' the code to the unspecified residual child
#'
#' @details Users should prefer `icd11_codeinfo()` while `icd11_codeinfo2` is for developers
#' of additional applications as it is directly the result from the API. The `icd11_codeinfo()`
#' makes two requests to identify the code in terms of a URI and then return information on
#' the URI
#'
#' @inheritParams icd11_autocode
#' @inheritParams icd11_id
#'
#' @name code_info
NULL

#' @rdname code_info
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   code_info <- icd11_codeinfo(token, code = "8B20")
#'
#'   #Use retrieve function to obtain more information
#'   code_info |> retrieve("title")
#'   code_info |> retrieve("definition")
#' }
#'
#' @export
icd11_codeinfo <- function(token, code, release = "2024-01", linearization = "mms",
                            convert_2_terminal = FALSE, include = NULL,
                            language = "en", auto_update = TRUE) {


  autocode <- icd11_codeinfo2(token = token, code = code, release = release,
                              linearization = linearization, flexible_mode = FALSE,
                              convert_2_terminal = convert_2_terminal,
                              language = language, auto_update = auto_update)


  icd11_id(token = token, id = autocode$stemId, linearization = linearization,
           release = release, language = language, auto_update = auto_update,
           include = include)
}

#' @rdname code_info
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   icd11_codeinfo2(token, code = "8B20")
#' }
#'
#' @export
icd11_codeinfo2 <- function(token, code, release = "2024-01", linearization = "mms",
                           flexible_mode = FALSE, convert_2_terminal = FALSE,
                           language = "en", auto_update = TRUE) {


  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/",release, "/", linearization,"/codeinfo/", code),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request or code not found",
    req_body = list(
      flexiblemode = flexible_mode,
      convertToTerminalCodes = convert_2_terminal
    )
  )

  return(autocode)
}

#' Looks up a foundation entity within a linearization
#'
#' Looks up a foundation entity within a linearization and returns where that entity is coded
#' in this linearization.
#'
#' @inheritParams icd11_id
#'
#' @note This function is mostly for development of extensions. End-users should refer to
#' `icd_search()`.
#'
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   icd11_lookup(token, id = "http://id.who.int/icd/entity/1442995018")
#' }
#'
#' @export
icd11_lookup <- function(token, id, release = "2024-01", linearization = "mms",
                         language = "en", auto_update = TRUE) {


  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/",release, "/", linearization,"/lookup"),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Release for request or code not found",
    req_body = list(
      foundationUri = id
    )
  )

  return(autocode)
}


#' Residual
#'
#' Returns the residual from a URI
#'
#' @inheritParams icd11_autocode
#' @inheritParams icd11_id
#' @param residual For residual categories: - this could be 'other' for other specified residual
#' category - or 'unspecified' for the unspecified residual category
#'
#' @name residual
NULL

#' @rdname residual
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   icd11_residual(token, id = "1442995018")
#'
#'}
#' @export
icd11_residual <- function(token, id, residual = "", linearization = "mms", release = "2024-01", include = NULL,
                           language = "en", auto_update = TRUE) {

  # Preprocess ID if URI is provided to keep only last digits
  parts <- strsplit(id, "/")[[1]]
  id    <- utils::tail(parts, n = 1)

  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", release, "/", linearization, "/", id, "/", residual),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Residual not found on release for id",
    req_body = list(
      include = paste0(include, collapse = ", ")
    )
  )

  return(autocode)
}

#' @rdname residual
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Generated token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Search for ICD-11 URI across multiple releases
#'   icd11_residual2(token, id = "1442995018")
#'
#'}
#' @export
icd11_residual2 <- function(token, id, residual = "", linearization = "mms", include = NULL,
                           language = "en", auto_update = TRUE) {

  # Preprocess ID if URI is provided to keep only last digits
  parts <- strsplit(id, "/")[[1]]
  id    <- utils::tail(parts, n = 1)

  autocode <- request_WHO(
    url = paste0("https://id.who.int/icd/release/11/", linearization, "/", id, "/", residual),
    token = token,
    language = language,
    auto_update = auto_update,
    warning_message_404 = "Residual not found on release for id",
    req_body = list(
      include = paste0(include, collapse = ", ")
    )
  )

  return(autocode)
}

