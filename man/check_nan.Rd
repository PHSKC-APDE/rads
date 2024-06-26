% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_nan.R
\name{check_nan}
\alias{check_nan}
\title{Check to see if a value is NA, NULL, or ""}
\usage{
check_nan(x)
}
\arguments{
\item{x}{character. Expects length of 1 when not NULL (or NA).}
}
\value{
logical
}
\description{
Check to see if a value is NA, NULL, or ""
}
\examples{
check_nan(NaN)
check_nan(NA)
check_nan(NULL)
check_nan('')
check_nan('x')
check_nan(10)

}
