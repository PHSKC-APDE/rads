% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi_qa.R
\name{chi_qa}
\alias{chi_qa}
\title{QA for CHI/Tableau ready standards using R.}
\usage{
chi_qa(
  chi_est = NULL,
  chi_meta = NULL,
  acs = F,
  ignore_trends = T,
  verbose = FALSE
)
}
\arguments{
\item{chi_est}{Name of a data.table or data.frame containing the prepared data to be pushed to SQL}

\item{chi_meta}{Name of a data.table or data.frame containing the metadata to be pushed to SQL}

\item{acs}{Logical. Indicates whether it is ACS data (which does not have / need varnames)}

\item{ignore_trends}{Logical. Indicates whether the time_trends column should be ignored when checking for missing data.}

\item{verbose}{Logical. Should the function be talkative?}
}
\value{
If there are no problems, a printed statement of success. Otherwise, it will stop and provide informative
feedback every time there is an error.
}
\description{
This functions seeks to ensure that the data being assessed meet the detailed CHI
Tableau Ready standards detailed on our SharePoint:
DPH-CommunityHealthIndicators >> CHIVizes >> CHI-Standards-TableauReady Output.xlsx
}
\details{
This function ensures that the structure of the data matches all CHI Tableau Ready
specifications. QA for data quality vis-à-vis previous production data, CHAT, or any other source,
must be performed separately
}
\keyword{CHI,}
\keyword{Production}
\keyword{Tableau,}
