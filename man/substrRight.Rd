% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{substrRight}
\alias{substrRight}
\title{Substring selection from the right to complement base R substr}
\usage{
substrRight(x, x.start, x.stop)
}
\arguments{
\item{x}{character}

\item{x.start}{digit to start (counting from the right)}

\item{x.stop}{digit to end  (counting from the right)}
}
\value{
character vector
}
\description{
Substring selection from the right to complement base R substr
}
\examples{
\donttest{
substrRight("Good morning!", 2, 8)
}
}
