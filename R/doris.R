#' Interface for the DORIS system of WHO
#'
#' Provides an interface for the DORIS [https://icd.who.int/doris](https://icd.who.int/doris)
#' system by the World Health Organization. Obtains the underlying cause of death using the
#' rules of the Doris system. The fields provided are fields of a standard death certificate.
#'
#' All of the parameters are optional. In case of a missing parameter don't use it.
#'
#' @note
#' IMPORTANT!. This endpoint is pre-release. The results may have issues and the API may change in
#' the next releases. Underlying cause of death detection using the rules of the Doris system
#' (https://icd.who.int/doris) The fields provided are fields of a standard death certificate.
#' You don't need to provide causeOfDeathUri fields if you provide causeOfDeathCode fields or
#' vice versa
#'
#' @param release Release version as listed in
#' [https://icd.who.int/docs/icd-api/SupportedClassifications/#supported-classifications-and-versions](https://icd.who.int/docs/icd-api/SupportedClassifications/#supported-classifications-and-versions)
#' the default is `2024-01`.
#'
#' @param sex Can be either numeric (`1` for `Male`, `2` for `Female` and `9` for `Unknown`) or can
#' be a character with those words.
#'
#' @param age Estimated age of the individual in ISO-8601 format. Use [iso_8601()] to create the
#' age in the appropriate format.
#'
#' @param date_of_birth Date of birth of the individual (date or datetime object)
#'
#' @param date_of_death Date of death of the individual (date or datetime object)
#'
#' @param cause_of_death_code_a String. Cause field A classification codes, comma separated.
#'
#' @param cause_of_death_code_b String. Cause field B classification codes, comma separated.
#'
#' @param cause_of_death_code_c String. Cause field C classification codes, comma separated.
#'
#' @param cause_of_death_code_d String. Cause field D classification codes, comma separated.
#'
#' @param cause_of_death_code_e String. Cause field E classification codes, comma separated.
#'
#' @param cause_of_death_uri_a String. Cause field A classification URIs, comma separated.
#'
#' @param cause_of_death_uri_b String. Cause field B classification URIs, comma separated.
#'
#' @param cause_of_death_uri_c String. Cause field C classification URIs, comma separated.
#'
#' @param cause_of_death_uri_d String. Cause field D classification URIs, comma separated.
#'
#' @param cause_of_death_uri_e String. Cause field E classification URIs, comma separated.
#'
#' @param cause_of_death_code_part2 String. Cause field Part2 classification codes, comma separated.
#'
#' @param cause_of_death_uri_part2 String. Cause field Part2 classification URIs, comma separated.
#'
#' @param interval_a Time interval from onset to death for Field A, in ISO 8601 format.
#' Use [iso_8601()] to construct that duration.
#'
#' @param interval_b Time interval from onset to death for Field B, in ISO 8601 format.
#' Use [iso_8601()] to construct that duration.
#'
#' @param interval_c Time interval from onset to death for Field C, in ISO 8601 format.
#' Use [iso_8601()] to construct that duration.
#'
#' @param interval_d Time interval from onset to death for Field D, in ISO 8601 format.
#' Use [iso_8601()] to construct that duration.
#'
#' @param interval_e Time interval from onset to death for Field E, in ISO 8601 format.
#' Use [iso_8601()] to construct that duration.
#'
#' @param surgery_was_performed Whether surgery was performed on the individual
#' (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param surgery_date Date of surgery on the individual if `surgery_was_performed`
#'
#' @param surgery_reason String. Reason for surgery.
#'
#' @param autopsy_was_requested Whether an autposy was performed on the individual
#' (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param autopsy_findings Whether findings from the autopsy were used in the
#' certification (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`).
#'
#' @param manner_of_death Either code or word: 0: Disease, 1: Accident, 2: Intentional self-harm,
#' 3: Assault, 4: Legal intervention, 5: War, 6: Could not be determined,
#' 7: Pending investigation, 9: Unknown.
#'
#' @param manner_of_death_date_of_external_cause_or_poisoning Date of external cause or poisoning of
#' the individual according to `manner_of_death`
#'
#' @param manner_of_death_description_external_cause String. Description of external cause.
#'
#' @param manner_of_death_place_of_occurrence_external_cause Integer. 0: At home, 1: Residential
#' institution, 2: School, other institution, public administration area, 3: Sports and athletics
#' area, 4: Street and highway, 5: Trade and service area, 6: Industrial and construction area,
#' 7: Farm, 8: Other place, 9: Unknown.
#'
#' @param fetal_or_infant_death_multiple_pregnancy (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param fetal_or_infant_death_stillborn (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param fetal_or_infant_death_within_24h Hours survived if death occurred within 24 hours.
#'
#' @param fetal_or_infant_death_birth_weight Birth weight in grams.
#'
#' @param fetal_or_infant_death_pregnancy_weeks Integer. Number of completed weeks of pregnancy.
#'
#' @param fetal_or_infant_death_age_mother Integer. Age of mother in years.
#'
#' @param fetal_or_infant_death_perinatal_description String. Condition of mother affecting the fetus and newborn.
#'
#' @param maternal_death_was_pregnant For women, was the deceased pregnant?
#' (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param maternal_death_time_from_pregnancy Integer. 0: At time of death,
#' 1: Within 42 days before death, 2: Between 43 days and 1 year before death,
#' 3: One year or more before death, 9: Unknown.
#'
#' @param maternal_death_pregnancy_contribute Did pregnancy contribute to death?
#' (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param api_version Version of the WHO API you are using. Don't change it unless you know what
#' you are doing.
#'
#' @param stop_on_error Whether to stop if API throws an error or just throw it as a warning.
#'
#' @inheritParams request_WHO
#' @inheritParams get_token
#' @examples
#' # Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'   # Get the Token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   # Check a 25 year old male with immediate cause 1C8Z
#'   # due to 1C62.3Z
#'   doris(token,
#'     sex = "Male", age = iso_8601(years = 25), cause_of_death_code_a = "1C8Z",
#'     cause_of_death_code_b = "1C62.3Z"
#'   )
#'
#'   # Check a 52 year old with immediate cause "GB61.Z" due to GB60.Z
#'   # due to 5A11 due to BA00.Z with other significant issues contributing to death
#'   # being 3A9Z and BC45
#'   doris(token,
#'     sex = "Male", age = iso_8601(years = 25), cause_of_death_code_a = "GB61.Z",
#'     cause_of_death_code_b = "GB60.Z", cause_of_death_code_c = "5A11",
#'     cause_of_death_code_d = "BA00.Z", cause_of_death_code_part2 = c("3A9Z", "BC45")
#'   )
#' }
#'
#' @returns List with the cause of death report
#' @export
doris <- function(token,
                  sex = NULL,
                  age = NULL,
                  date_of_birth = NULL,
                  date_of_death = NULL,
                  cause_of_death_code_a = NULL,
                  cause_of_death_code_b = NULL,
                  cause_of_death_code_c = NULL,
                  cause_of_death_code_d = NULL,
                  cause_of_death_code_e = NULL,
                  cause_of_death_uri_a = NULL,
                  cause_of_death_uri_b = NULL,
                  cause_of_death_uri_c = NULL,
                  cause_of_death_uri_d = NULL,
                  cause_of_death_uri_e = NULL,
                  cause_of_death_code_part2 = NULL,
                  cause_of_death_uri_part2 = NULL,
                  interval_a = NULL,
                  interval_b = NULL,
                  interval_c = NULL,
                  interval_d = NULL,
                  interval_e = NULL,
                  surgery_was_performed = NULL,
                  surgery_date = NULL,
                  surgery_reason = NULL,
                  autopsy_was_requested = NULL,
                  autopsy_findings = NULL,
                  manner_of_death = NULL,
                  manner_of_death_date_of_external_cause_or_poisoning = NULL,
                  manner_of_death_description_external_cause = NULL,
                  manner_of_death_place_of_occurrence_external_cause = NULL,
                  fetal_or_infant_death_multiple_pregnancy = NULL,
                  fetal_or_infant_death_stillborn = NULL,
                  fetal_or_infant_death_within_24h = NULL,
                  fetal_or_infant_death_birth_weight = NULL,
                  fetal_or_infant_death_pregnancy_weeks = NULL,
                  fetal_or_infant_death_age_mother = NULL,
                  fetal_or_infant_death_perinatal_description = NULL,
                  maternal_death_was_pregnant = NULL,
                  maternal_death_time_from_pregnancy = NULL,
                  maternal_death_pregnancy_contribute = NULL,
                  release = "2024-01",
                  language = "en",
                  api_version = "v2",
                  stop_on_error = FALSE) {
  # Do a request for release
  req <- httr2::request(
    base_url = paste0("https://id.who.int/icd/release/11/", release, "/doris")
  ) |>
    httr2::req_headers(
      Authorization = paste(token["token_type"], token["access_token"]),
      Accept = "application/json",
      `Accept-Language` = "en",
      `API-Version` = "v2"
    ) |>
    httr2::req_url_query(
      sex = codify_sex(sex),
      estimatedAge = age,
      dateBirth = date_of_birth,
      dateDeath = date_of_death,
      causeOfDeathCodeA = paste0(cause_of_death_code_a, collapse = ", "),
      causeOfDeathCodeB = paste0(cause_of_death_code_b, collapse = ", "),
      causeOfDeathCodeC = paste0(cause_of_death_code_c, collapse = ", "),
      causeOfDeathCodeD = paste0(cause_of_death_code_d, collapse = ", "),
      causeOfDeathCodeE = paste0(cause_of_death_code_e, collapse = ", "),
      causeOfDeathUriA = paste0(cause_of_death_uri_a, collapse = ", "),
      causeOfDeathUriB = paste0(cause_of_death_uri_b, collapse = ", "),
      causeOfDeathUriC = paste0(cause_of_death_uri_c, collapse = ", "),
      causeOfDeathUriD = paste0(cause_of_death_uri_d, collapse = ", "),
      causeOfDeathUriE = paste0(cause_of_death_uri_e, collapse = ", "),
      causeOfDeathCodePart2 = paste0(cause_of_death_code_part2, collapse = ", "),
      causeOfDeathUriPart2 = paste0(cause_of_death_uri_part2, collapse = ", "),
      intervalA = interval_a,
      intervalB = interval_b,
      intervalC = interval_c,
      intervalD = interval_d,
      intervalE = interval_e,
      surgeryWasPerformed = codify_ans(surgery_was_performed),
      surgeryDate = surgery_date,
      surgeryReason = surgery_reason,
      autopsyWasRequested = codify_ans(autopsy_was_requested),
      autopsyFindings = codify_ans(autopsy_findings),
      mannerOfDeath = codify_manner_of_death(manner_of_death),
      mannerOfDeathDateOfExternalCauseOrPoisoning = manner_of_death_date_of_external_cause_or_poisoning,
      mannerOfDeathDescriptionExternalCause = manner_of_death_description_external_cause,
      mannerOfDeathPlaceOfOccuranceExternalCause = code_place(manner_of_death_place_of_occurrence_external_cause),
      fetalOrInfantDeathMultiplePregnancy = codify_ans(fetal_or_infant_death_multiple_pregnancy),
      fetalOrInfantDeathStillborn = codify_ans(fetal_or_infant_death_stillborn),
      fetalOrInfantDeathWithin24h = fetal_or_infant_death_within_24h,
      fetalOrInfantDeathBirthWeight = fetal_or_infant_death_birth_weight,
      fetalOrInfantDeathPregnancyWeeks = fetal_or_infant_death_pregnancy_weeks,
      fetalOrInfantDeathAgeMother = fetal_or_infant_death_age_mother,
      fetalOrInfantDeathPerinatalDescription = fetal_or_infant_death_perinatal_description,
      maternalDeathWasPregnant = codify_ans(maternal_death_was_pregnant),
      maternalDeathTimeFromPregnancy = code_maternal_death_time_from_pregnancy(maternal_death_time_from_pregnancy),
      maternalDeathPregnancyContribute = codify_ans(maternal_death_pregnancy_contribute)
    )

  # Perform the request
  request <- httr2::req_perform(req = req)

  report <- request |> resp_body_json()

  # Check for report's warnings and errors
  warning_msn <- unlist(report["warning"])
  if (!is.null(warning_msn)) {
    warning(warning_msn)
  }

  error_msn <- unlist(report["error"])
  if (stop_on_error & !is.null(error_msn)) {
    stop(error_msn)
  } else if (!is.null(error_msn)) {
    warning(error_msn)
  }

  # Change the names according to doris website
  names(report)[which(names(report) == "stemCode")] <- "Underlying cause of death (UCOD)"
  names(report)[which(names(report) == "stemURI")] <- "UCOD URI"
  names(report)[which(names(report) == "code")] <- "UCOD with postcoordinated information (if available)"
  names(report)[which(names(report) == "uri")] <- "UCOD postcoordinated URI"
  names(report)[which(names(report) == "report")] <- "Detailed explanation (report)"

  # Return the report
  return(report)
}
