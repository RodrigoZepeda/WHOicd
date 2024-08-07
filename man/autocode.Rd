% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd11.R
\name{autocode}
\alias{autocode}
\alias{icd11_autocode}
\alias{icd11_autocode2}
\title{Autocode}
\usage{
icd11_autocode(
  token,
  text,
  release = "2024-01",
  linearization = "mms",
  subtrees_filter = NULL,
  match_threshold = NULL,
  language = "en",
  auto_update = TRUE
)

icd11_autocode2(
  token,
  text,
  release = "2024-01",
  subtrees_filter = NULL,
  match_threshold = NULL,
  language = "en",
  auto_update = TRUE
)
}
\arguments{
\item{token}{(\code{list}) Access token to the API obtained from \code{\link[=get_token]{get_token()}}}

\item{text}{(stirng) String to autocode with ICD-11}

\item{release}{Release version as listed in
\url{https://icd.who.int/docs/icd-api/SupportedClassifications/#supported-classifications-and-versions}
the default is \code{2024-01}.}

\item{linearization}{(\code{character}) ICD-11 linearization (\code{default = "mms"}). See all linearizations
in the WHO's API website \url{https://icd.who.int/docs/icd-api/APIDoc-Version2/#the-icd11-linearizations}}

\item{subtrees_filter}{(string) Optional parameter. Vector of URIs. If provided,
the search will be performed on the entities provided and their descendants}

\item{match_threshold}{(string) Score is a value between 0 and 1 that indicates the similarity
between the input text and the matched term. \code{match_threshold} is the minimum score to be
included in the output. Will use WHO's default value if no threshold provided}

\item{language}{(\code{character}) Language for the request. Available languages are
listed in \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}
the default is English (\code{language = "en"}).}

\item{auto_update}{Attempts to update token automatically}
}
\value{
A list of the coded information as well as any additional information.
}
\description{
Provides the best matching classification entity (its code and URI) for the
provided diagnostic text.
}
\details{
The \code{icd11_autocode} function allows you to search across different linearizations such
as the Mortality and Morbidity Statistics \code{linearization = "mms"} (default) and the
International Classifications of Functioning Disability and Health \code{linearization = "icf"}.
}
\note{
Both \code{icd11_autocode()} and \code{icd11_autocode2()} provide the best classification. They
correspond to two different API entry-points in the WHO's API and provide slighly different
amount of information for the same result.

This provides the default autocode but might throw different amount of information
as \code{icd11_autocode2}.
}
\examples{
# Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  # Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Search for ICD-10 releases
  icd11_autocode(token, text = "Cerebrovascular accident")
}

# Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  # Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Search for ICD-10 releases
  icd11_autocode2(token, text = "Cerebrovascular accident")
}

}
