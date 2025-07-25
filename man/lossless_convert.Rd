% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{lossless_convert}
\alias{lossless_convert}
\title{Convert the class of a vector to another class -- when possible without
introducing additional NAs}
\usage{
lossless_convert(x, class, column_name = NULL)
}
\arguments{
\item{x}{vector of indeterminate length and type}

\item{class}{character vector of length one specifying the preferred new column type (e.g.,
'character', 'numeric', 'integer', 'Date', or 'POSIXct)}

\item{column_name}{optional name of the column being converted (for better error messages)}
}
\value{
a vector of the same length as x, but of the new class (when possible)
}
\description{
Convert the class of a vector to another class -- when possible without
introducing additional NAs. If NAs would be introduced, the original vector
will be returned along with a warning so the user knows it has not been
converted.
}
\examples{
\donttest{
# Create a bunch of sample vectors
alpha <- c('2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01')
beta <- c(NA, '2023-01-01', '2024-01-01', '2025-01-01')
gamma <- c(NA, 'Not a Date', '2024-01-01', '2025-01-01')
delta <- c('Not a Date', '2023-01-01', '2024-01-01', '2025-01-01')
epsilon <- c('1', '2', '3', NA)
zeta <- c('One', '2', '3', NA)
eta <- c('1.1', '2', '3', NA)
tau <- c(NA, '2023-01-01 12:30:45', '2024-12-31 23:59:59', '2025-01-01 11:11:11')

# Successful Date conversion
inherits(lossless_convert(alpha, 'Date'), 'Date')

# Failed Date conversion (preserves original)
inherits(lossless_convert(gamma, 'Date'), 'character')

# Successful integer conversion
inherits(lossless_convert(epsilon, 'integer'), 'integer')

# Failed integer conversion
inherits(lossless_convert(eta, 'integer'), 'character')

# Successful POSIXct conversion
inherits(lossless_convert(tau, 'POSIXct'), 'POSIXct')

# Convert all possible columns in a data.table to numeric
library(data.table)
mydt <- data.table(alpha, beta, gamma, delta, epsilon, zeta, eta)
mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
  lossless_convert(get(col_name), class = 'numeric', column_name = col_name)
})]
all.equal(names(mydt)[sapply(mydt, is.numeric)], c('epsilon', 'eta'))

# Convert all possible columns in a data.table to Date
mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
  lossless_convert(get(col_name), class = 'Date', column_name = col_name)
})]
all.equal(names(mydt)[sapply(mydt, function(x) inherits(x, "Date"))], c("alpha", "beta"))
}

}
