% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{format_time_simple}
\alias{format_time_simple}
\title{Format a vector of time (or any numeric values) into a single human readable chunk}
\usage{
format_time_simple(x)
}
\arguments{
\item{x}{numeric}
}
\value{
character vector
}
\description{
Format a vector of time (or any numeric values) into a single human readable chunk
}
\examples{
format_time_simple(c(1:5, 10, 12, 24, 25))

}
