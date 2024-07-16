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
#'   dbf <- .icd10_search_recursive(token, c("XI","XII", NA_character_))
#'   dbf <- .icd10_search_recursive(token, c("D50-D53", "D50-D53", NA_character_, "D60-D64"))
#'
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
