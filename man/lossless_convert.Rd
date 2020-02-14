% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{lossless_convert}
\alias{lossless_convert}
\title{Convert the class of a vector to another class is possible without introducing additional NAs}
\usage{
lossless_convert(x = NULL, class = NULL)
}
\arguments{
\item{x}{vector of indeterminate length and type}

\item{class}{character vector of length one specifying the preferred new column type (i.e.,
'character', 'numeric', 'integer', or 'factor')}
}
\value{
a vector of the same length as x, but of the new class (when possible)
}
\description{
Convert the class of a vector to another class is possible without introducing additional NAs
}