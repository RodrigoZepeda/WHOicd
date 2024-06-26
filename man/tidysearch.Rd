% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd10.R
\name{tidysearch}
\alias{tidysearch}
\alias{icd10_code_info_tidy}
\alias{icd10_block_info_tidy}
\alias{icd10_chapter_info_tidy}
\title{Search for information on multiple codes, blocks or chapter in a
\code{data.frame} or \code{tibble}.}
\usage{
icd10_code_info_tidy(
  .data,
  colname,
  token,
  release = 2019,
  language = "en",
  dry_run = FALSE,
  codes_only = FALSE,
  validate_code = TRUE
)

icd10_block_info_tidy(
  .data,
  colname,
  token,
  release = 2019,
  language = "en",
  dry_run = FALSE,
  codes_only = FALSE,
  validate_code = TRUE
)

icd10_chapter_info_tidy(
  .data,
  colname,
  token,
  release = 2019,
  language = "en",
  dry_run = FALSE,
  codes_only = FALSE,
  validate_code = TRUE
)
}
\arguments{
\item{.data}{A \code{data.frame} or a \code{data.frame}extension such as a \code{tibble}.}

\item{colname}{(\code{character}) Name of the \code{data.frame} column with the ICD-10 code/block or chapter
to search}

\item{token}{(\code{list}) Access token to the API obtained from \code{\link[=get_token]{get_token()}}}

\item{release}{(\code{character}) ICD-10 release (\code{default = 2019}). Use
\code{\link[=icd10_releases]{icd10_releases()}} to get all available releases.}

\item{language}{(\code{character}) Language for the request. Available languages are
listed in \url{https://icd.who.int/docs/icd-api/SupportedClassifications/}
the default is English (\code{language = "en"}).}

\item{dry_run}{(\code{logical}) If \code{TRUE} will create the request and generate an empty token. This
function is mostly for development.}

\item{codes_only}{Return only the codes without the titles. This option
is faster as it realizes less requests.}

\item{validate_code}{( \code{logical}) Apply additional functions to validate that
the ICD-10 code is written as the API needs it.}
}
\description{
Search for information on multiple codes, blocks or chapter in a
\code{data.frame} or \code{tibble}.
}
\examples{
#Change `dry_run = FALSE` to run the example
token <- get_token("123","123", dry_run = TRUE)
codes_df <- data.frame(Sex = c("M","F","F"), icd10 = c("E14.1","C80.0","F14"))
codes_df |>
    icd10_code_info_tidy("icd10", token, dry_run = TRUE)
#Change `dry_run = FALSE` to run the example
token <- get_token("123","123", dry_run = TRUE)
codes_df <- data.frame(Sex = c("M","F","F"), icd10 = c("C76-C80","E10-E14","F10-F19"))
codes_df |>
    icd10_block_info_tidy("icd10", token, dry_run = TRUE)
#Change `dry_run = FALSE` to run the example
token    <- get_token("123","123", dry_run = TRUE)
codes_df <- data.frame(Sex = c("M","F","F"), icd10_chapter = c("X", "IV","II"))
codes_df |>
    icd10_chapter_info_tidy("icd10_chapter", token, dry_run = TRUE)
}
