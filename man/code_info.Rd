% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd11.R
\name{code_info}
\alias{code_info}
\alias{icd11_codeinfo}
\alias{icd11_codeinfo2}
\title{Code info}
\usage{
icd11_codeinfo(
  token,
  code,
  release = "2024-01",
  linearization = "mms",
  convert_2_terminal = FALSE,
  include = NULL,
  language = "en",
  auto_update = TRUE
)

icd11_codeinfo2(
  token,
  code,
  release = "2024-01",
  linearization = "mms",
  flexible_mode = FALSE,
  convert_2_terminal = FALSE,
  language = "en",
  auto_update = TRUE
)
}
\arguments{
\item{token}{(\code{list}) Access token to the API obtained from \code{\link[=get_token]{get_token()}}}

\item{code}{The code or code combination that will be looked up. (The & and / characters need
to be URL encoded)}

\item{release}{Release version as listed in
\url{https://icd.who.int/docs/icd-api/SupportedClassifications/#supported-classifications-and-versions}
the default is \code{2024-01}.}

\item{linearization}{(\code{character}) ICD-11 linearization (\code{default = "mms"}). See all linearizations
in the WHO's API website \url{https://icd.who.int/docs/icd-api/APIDoc-Version2/#the-icd11-linearizations}}

\item{convert_2_terminal}{by default it is \code{FALSE}, if set to \code{TRUE}, the codes will be changed
to use the terminal codes (leaf nodes in the hierarchy). In most cases this is done by changing
the code to the unspecified residual child}

\item{include}{Either NULL of provide additional information such as \code{ancestor},
\code{decendant} or \code{diagnosticCriteria}. For example, if you set \code{include="ancestor"} the returned
response will include all ancestor entity URIs for the requested entity. Use a vector to include
multiple properties. For example \code{include = c("ancestor", "diagnosticCriteria")}}

\item{language}{(\code{character}) Language for the request. Available languages are
listed in \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}
the default is English (\code{language = "en"}).}

\item{auto_update}{Attempts to update token automatically}

\item{flexible_mode}{Normally, \code{icd11_codeinfo} returns an error when the provided code combination
includes codes that are not in the predefined suggested value sets of the entity. If the
\code{flexible_mode} is set to \code{TRUE}, it will return a result even when some of the postcoordination
values are not in these defined value sets. In this case, the codes whose postcoordination axis
cannot be detected ('/' postcoordination) will be placed under otherPostcoordination rubric.}
}
\description{
Returns information on a certain code.
}
\details{
Users should prefer \code{icd11_codeinfo()} while \code{icd11_codeinfo2} is for developers
of additional applications as it is directly the result from the API. The \code{icd11_codeinfo()}
makes two requests to identify the code in terms of a URI and then return information on
the URI
}
\examples{
# Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  # Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Search for ICD-11 URI across multiple releases
  code_info <- icd11_codeinfo(token, code = "8B20")

  #Use retrieve function to obtain more information
  code_info |> retrieve("title")
  code_info |> retrieve("definition")
}

# Assuming that the CLIENT ID and CLIENT SECRET are set up. Substitute accordingly
if (exists("CLIENT_ID") & exists("CLIENT_SECRET")) {
  # Generated token
  token <- get_token(CLIENT_ID, CLIENT_SECRET)

  # Search for ICD-11 URI across multiple releases
  icd11_codeinfo2(token, code = "8B20")
}

}
