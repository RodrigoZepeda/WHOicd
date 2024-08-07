% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{code_maternal_death_time_from_pregnancy}
\alias{code_maternal_death_time_from_pregnancy}
\title{Verify and Convert Maternal Death Time from Pregnancy Code}
\usage{
code_maternal_death_time_from_pregnancy(input)
}
\arguments{
\item{input}{Either a number or a string representing the maternal death time from pregnancy code.}
}
\value{
The corresponding maternal death time from pregnancy code as a number, or an error
if the input is invalid.
}
\description{
This function takes one parameter, which can be a number or a string. If it is a number,
the function verifies it against predefined codes. If it is not one of the codes, it returns an error.
If it is a text, the function returns the number corresponding to the code. If it is NULL,
the function returns NULL.
}
\keyword{internal}
