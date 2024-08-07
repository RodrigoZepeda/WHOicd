% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd10.R
\name{icd10_release_info}
\alias{icd10_release_info}
\title{Request information on a certain ICD-10 release}
\usage{
icd10_release_info(token, release = 2019, language = "en", auto_update = TRUE)
}
\arguments{
\item{token}{(\code{list}) Access token to the API obtained from \code{\link[=get_token]{get_token()}}}

\item{release}{(\code{character}) ICD-10 release (\code{default = 2019}). Use
\code{\link[=icd10_releases]{icd10_releases()}} to get all available releases.}

\item{language}{(\code{character}) Language for the request. Available languages are
listed in \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}
the default is English (\code{language = "en"}).}

\item{auto_update}{Attempts to update token automatically}
}
\value{
vector with information on the publishing date and title
}
\description{
Obtains publishing information available for a specific
release of the International Classification of Diseases ICD-10.
}
\examples{
#Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  #Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Get title and url for release
  icd10_release_info(token, release = 2016)
}

}
