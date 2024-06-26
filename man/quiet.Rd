% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{quiet}
\alias{quiet}
\title{Silence (i.e., suppress or mute) printed messages from functions}
\usage{
quiet(expr, suppressWarnings = FALSE)
}
\arguments{
\item{expr}{the expression that you desire to silence (i.e., the function
along with its arguments)}

\item{suppressWarnings}{a logical (TRUE or FALSE), noting whether you wish
to suppress warning messages. The default is \code{suppressWarnings = FALSE}}
}
\value{
whatever should be returned by the expression that is being silenced
}
\description{
Silence messages from noisy functions. Optionally silence warning messages
too.
}
\examples{
\donttest{
# Suppresses only messages
result <- quiet({
  message("This message is silenced")
  warning("This warning is shown")
  42  # Return a value
})

# Suppresses both messages and warnings
result <- quiet({
  message("This message is silenced")
  warning("This warning is silenced too")
  42  # Return a value
}, suppressWarnings = TRUE)

}
}
\keyword{quiet}
\keyword{quietly}
\keyword{silence}
\keyword{silent}
