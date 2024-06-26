% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/death_functions.R
\name{death_injury_matrix_count}
\alias{death_injury_matrix_count}
\title{Generate a death injury matrix from line-level death data}
\source{
\code{rads.data::icd10_death_injury_matrix}
}
\usage{
death_injury_matrix_count(
  ph.data,
  intent = "*",
  mechanism = "*",
  icdcol = "underlying_cod_code",
  kingco = TRUE,
  group_by = NULL,
  ypll_age = NULL,
  death_age_col = NULL
)
}
\arguments{
\item{ph.data}{a data.table or data.frame. Must contain death data structured
with one person per row and with at least one column of ICD10 death codes.}

\item{intent}{a character vector of length 1 to 5. It specifies the
intent of death that you want returned ("Unintentional", "Suicide", "Homicide",
"Undetermined", or "Legal intervention/war"). "none" will ignore the intent
and only return the mechanism of death.

\strong{NOTE}
You do not have to type the entire keyword for the intent, a
partial string match is sufficient and is case insensitive. E.g.,
\code{intent = c("cide")} would return both "Suicide" and "Homicide" and
\code{intent = c("un")} would return both "Unintentional" and "Undetermined".

The default is \code{'*'}, which selects all possible intents.}

\item{mechanism}{a character vector of length 1 to 28. It specifies the
mechanism of death that you want returned (E.g., "Cut/pierce", "Drowning",
"Fall", "Firearm", etc.). "none" will ignore the mechanism
and only return the intent of death.

To see the complete list of mechanisms, type
\code{unique(rads.data::icd10_death_injury_matrix$mechanism)} in your
R console.

\strong{NOTE}
You do not have to type the entire keyword for the mechanism, a
partial string match is sufficient and is case insensitive. E.g.,
\code{mechanism = c("cycl")} would return both "Pedal cyclist" and
"Motorcyclist".

The default is \code{'*'}, which selects all possible mechanisms}

\item{icdcol}{a character vector of length one that specifies the name of the
column in ph.data that contains the ICD10 death codes of interest.

The default is \code{underlying_cod_code}, which is found in the properly
formatted death data obtained using the \code{get_data_death()} function.}

\item{kingco}{a logical vector of length one. It specifies whether you want to
limit the analysis to King County. Note that this only works with data
imported from the \code{get_data_death()} function.

The default is \code{kingco = TRUE}.}

\item{group_by}{a character vector of indeterminate length. This is used to
specify all the variables by which you want to group (a.k.a. stratify) the
results. For example, if you specified \code{group_by = c('chi_sex',
'chi_race_6')}, the results would be stratified by each combination of sex
and race.

The default is \code{group_by = NULL}}

\item{ypll_age}{an optional numeric vector of length 1. When specified, it
should be the age (an integer) used for \strong{Years of Potential Life Lost} (YPLL)
calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
common. For example, \code{ypll_age = 65} would sum the total number of years
that could have been lived had everyone in the data lived to at least 65.
Note that this function returns the total number of YPLL. Additional
processing is necessary to calculate rates per 100,000.

The default is \code{ypll_age = NULL}, which will skip YPLL calculations.}

\item{death_age_col}{an optional character vector of length one that specifies
the name of the column in ph.data with the decedents' age at death
in years. It is only needed if \code{ypll_age} is
specified AND if ph.data lacks a column named \code{chi_age}.

The default is \code{death_age_col = NULL}.}
}
\value{
The function returns a data.table with a minimum of three columns:
\code{mechanism}, \code{intent}, & \code{deaths}. \code{ypll_##} and
\code{group_by} columns will also be returned if specified in the arguments.
}
\description{
Generate death counts for a death injury matrix
specifying the intent and mechanism of injury. Needs line-level death data
with a properly formatted ICD10 column.
}
\details{
The matrix coding is based on the ICE (International Collaborative Effort on
Injury Statistics) standard.
}
\note{
The function default is to return the matrix of all intents and mechanisms
of death. You can choose to only return the intent or only return the
mechanism. If you set both to "none", you will receive a summary of all
injury deaths without regard to the intent.

Also note that terrorism codes (U01.#, U02.#, & U03.#) are not included
because they are not included in the coding used by WA DOH. If they are
needed, they can be obtained from the CDC link below.
}
\examples{
# create synthetic line level data
set.seed(98104)
injurydata <- data.table::data.table(
  cod.icd10 = c(
    # Cut/pierce, Homicide
    rep("X99", round(runif(1, 30, 10000), 0)),
    # Drowning, Unintentional
    rep("W65", round(runif(1, 30, 10000), 0)),
    # Fall, Suicide
    rep("X80", round(runif(1, 30, 10000), 0)),
    # Fire/flame, Undetermined
    rep("Y26", round(runif(1, 30, 10000), 0)),
    # Firearm, Legal intervention/war
    rep("Y350", round(runif(1, 30, 10000), 0)),
    # Poisoning, Unintentional
    rep("X40", round(runif(1, 30, 10000), 0)),
    # Overexertion, Unintentional
    rep("X50", round(runif(1, 30, 10000), 0)),
    # Other land transport, Homicide
    rep("Y03", round(runif(1, 30, 10000), 0)),
    # Pedal cyclist, other, Unintentional
    rep("V10", round(runif(1, 30, 10000), 0)))
)

injurydata[, year := sample(2015:2020, nrow(injurydata), replace = TRUE)]

# example 1: every available combination of mechanism and intent
eg1 <- death_injury_matrix_count(ph.data = injurydata,
                            intent = "*",
                            mechanism = "*",
                            icdcol = "cod.icd10",
                            kingco = FALSE,
                            ypll_age = NULL,
                            death_age_col = NULL)
head(eg1) # note the data are stratified by year because year was in ph.data

# example 2: falls designated as homicides and or suicides
eg2 <- death_injury_matrix_count(ph.data = injurydata,
                            intent = "icide",
                            mechanism = "fall",
                            icdcol = "cod.icd10",
                            kingco = FALSE,
                            ypll_age = NULL,
                            death_age_col = NULL)
head(eg2)

# example 3: summary of all injury deaths regardless of intent and mechanism
eg3 <- death_injury_matrix_count(ph.data = injurydata,
                            intent = "none",
                            mechanism = "none",
                            icdcol = "cod.icd10",
                            kingco = FALSE,
                            ypll_age = NULL,
                            death_age_col = NULL)
eg3[]

# example 4: any intent and mechanism with YPLL_65 given death_age_col
injurydata4 <- data.table::copy(injurydata)
set.seed(98104)
injurydata4[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
1:nrow(injurydata4)] # synthetic age of death
eg4 <- death_injury_matrix_count(ph.data = injurydata4,
                            intent = "none",
                            mechanism = "none",
                            icdcol = "cod.icd10",
                            kingco = FALSE,
                            ypll_age = 65,
                            death_age_col = "ageofdeath")
eg4[]

# example 5: all suicides, regardless of mechanism, stratified by age

injurydata5 <- data.table::copy(injurydata4)

eg5 <- death_injury_matrix_count(ph.data = injurydata5,
                            intent = "suicide",
                            mechanism = "none",
                            icdcol = "cod.icd10",
                            kingco = FALSE,
                            group_by = 'ageofdeath',
                            ypll_age = NULL,
                            death_age_col = NULL)
eg5[]
}
\references{
WA DOH CHAT: \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/CHATInjury(ICE)codes.pdf}

CDC: \url{https://www.cdc.gov/nchs/data/ice/icd10_transcode.pdf}
}
