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
#' @param interval_a String. Time interval from onset to death for Field A, in ISO 8601 format.
#'
#' @param interval_b String. Time interval from onset to death for Field B, in ISO 8601 format.
#'
#' @param interval_c String. Time interval from onset to death for Field C, in ISO 8601 format.
#'
#' @param interval_d String. Time interval from onset to death for Field D, in ISO 8601 format.
#'
#' @param interval_e String. Time interval from onset to death for Field E, in ISO 8601 format.
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
#' @param autopsy_findings (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param manner_of_death Integer. 0: Disease, 1: Accident, 2: Intentional self-harm, 3: Assault,
#' 4: Legal intervention, 5: War, 6: Could not be determined, 7: Pending investigation, 9: Unknown.
#'
#' @param manner_of_death_date_of_external_cause_or_poisoning Date of external cause or poisoning of the individual
#' if `manner_of_death` is `1`, `2`, `3`, `4`, `5`, `6`,
#'
#' @param manner_of_death_description_external_cause String. Description of external cause.
#'
#' @param manner_of_death_place_of_occurrence_external_cause Integer. 0: At home, 1: Residential institution, 2: School, other institution, public administration area, 3: Sports and athletics area, 4: Street and highway, 5: Trade and service area, 6: Industrial and construction area, 7: Farm, 8: Other place, 9: Unknown.
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
#' @param maternal_death_time_from_pregnancy Integer. 0: At time of death, 1: Within 42 days before death, 2: Between 43 days and 1 year before death, 3: One year or more before death, 9: Unknown.
#'
#' @param maternal_death_pregnancy_contribute Did pregnancy contribute to death?
#' (`0` or `No`, `1` or `Yes`, and `9` or `Unknown`)
#'
#' @param api_version Version of the WHO API you are using. Don't change it unless you know what
#' you are doing.
#'
#' @inheritParams request_WHO
#' @inheritParams get_token
#' @examples
#' #Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
#' if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
#'
#'   #Get the Token
#'   token <- get_token(CLIENT_ID, CLIENT_SECRET)
#'
#'   #Check a 25 year old male with immediate cause 1C8Z
#'   #due to 1C68.3
#'   doris(token, sex = 1, age = iso_8601(years = 25), cause_of_death_code_a = "1C8Z",
#'         cause_of_death_code_b = "1C62.3Z")
#'
#' }

#' @returns List with the cause of death report
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
                  api_version = "v2") {

  #Do a request for release
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
      sex = sex,
      estimatedAge = age,
      dateBirth = date_of_birth,
      dateDeath = date_of_death,
      causeOfDeathCodeA = cause_of_death_code_a,
      causeOfDeathCodeB = cause_of_death_code_b,
      causeOfDeathCodeC = cause_of_death_code_c,
      causeOfDeathCodeD = cause_of_death_code_d,
      causeOfDeathCodeE = cause_of_death_code_e,
      causeOfDeathUriA = cause_of_death_uri_a,
      causeOfDeathUriB = cause_of_death_uri_b,
      causeOfDeathUriC = cause_of_death_uri_c,
      causeOfDeathUriD = cause_of_death_uri_d,
      causeOfDeathUriE = cause_of_death_uri_e,
      causeOfDeathCodePart2 = cause_of_death_code_part2,
      causeOfDeathUriPart2 = cause_of_death_uri_part2,
      intervalA = interval_a,
      intervalB = interval_b,
      intervalC = interval_c,
      intervalD = interval_d,
      intervalE = interval_e,
      surgeryWasPerformed = surgery_was_performed,
      surgeryDate = surgery_date,
      surgeryReason = surgery_reason,
      autopsyWasRequested = autopsy_was_requested,
      autopsyFindings = autopsy_findings,
      mannerOfDeath = manner_of_death,
      mannerOfDeathDateOfExternalCauseOrPoisoning = manner_of_death_date_of_external_cause_or_poisoning,
      mannerOfDeathDescriptionExternalCause = manner_of_death_description_external_cause,
      mannerOfDeathPlaceOfOccuranceExternalCause = manner_of_death_place_of_occurrence_external_cause,
      fetalOrInfantDeathMultiplePregnancy = fetal_or_infant_death_multiple_pregnancy,
      fetalOrInfantDeathStillborn = fetal_or_infant_death_stillborn,
      fetalOrInfantDeathWithin24h = fetal_or_infant_death_within_24h,
      fetalOrInfantDeathBirthWeight = fetal_or_infant_death_birth_weight,
      fetalOrInfantDeathPregnancyWeeks = fetal_or_infant_death_pregnancy_weeks,
      fetalOrInfantDeathAgeMother = fetal_or_infant_death_age_mother,
      fetalOrInfantDeathPerinatalDescription = fetal_or_infant_death_perinatal_description,
      maternalDeathWasPregnant = maternal_death_was_pregnant,
      maternalDeathTimeFromPregnancy = maternal_death_time_from_pregnancy,
      maternalDeathPregnancyContribute = maternal_death_pregnancy_contribute
    )

  #Perform the request
  request <- httr2::req_perform(req = req)

  report <- request |> resp_body_json()

  #Return the report
  return(report)
}
