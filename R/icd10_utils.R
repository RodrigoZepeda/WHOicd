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
icd10_validate_code <- function(code) {
  # Capitalize
  code <- toupper(code)

  # Check that it has a point if the number of characters for ICD-10 code is
  # larger than 3
  if (nchar(code) > 3) {
    if (substring(code, 4, 4) != ".") {
      code <- paste0(substring(code, 1, 3), ".", substring(code, 4))
    }
  }

  return(code)
}
