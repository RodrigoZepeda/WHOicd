% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd11.R
\name{icd11_release_info}
\alias{icd11_release_info}
\title{Request information on ICD-11 release}
\usage{
icd11_release_info(
  token,
  release = "2024-01",
  language = "en",
  auto_update = TRUE
)
}
\arguments{
\item{token}{(\code{list}) Access token to the API obtained from \code{\link[=get_token]{get_token()}}}

\item{release}{(\code{character}) ICD-11 release (\code{default = 2024-01}). See all releases
in the WHO's website \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}}

\item{language}{(\code{character}) Language for the request. Available languages are
listed in \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}
the default is English (\code{language = "en"}).}

\item{auto_update}{Attempts to update token automatically}
}
\value{
list with information on the publishing date and title
}
\description{
Obtains publishing information on the International Classification of Diseases
ICD-11.
}
\examples{
# Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  # Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Get title and url for release
  icd11_release_info(token, release = "2023-01")
}

}
