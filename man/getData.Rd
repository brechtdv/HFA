\name{getData}
\alias{getData}

\title{Get Data}

\description{
Get data by indicator, year, country, region.
}

\usage{
getData(indicator, year = NULL, country = NULL, region = NULL)
}

\arguments{
  \item{indicator}{ID (character) of the desired indicators; one or more IDs are allowed.}
  \item{year}{year (numeric or character) for which data are desired; one or more years allowed. The default value, \code{NULL}, leads to the use of all available years.}
  \item{country}{ID (character) of the desired countries; one or more IDs are allowed. The default value, \code{NULL}, leads to the use of all available countries.}
  \item{region}{ID (numeric or character) of the desired region; only one ID is allowed.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with five columns: \code{Yr}, \code{IndID}, \code{RegID}, \code{CntrID}, and \code{val}.
}

\seealso{
\code{\link{getIndicatorGroups}} to extract the set of available indicator groups\cr
\code{\link{getIndicators}} to extract the set of available indicators\cr
\code{\link{findIndicators}} to find indicators based on a text string\cr
\code{\link{getDefinitions}} to extract the definitions of the indicators\cr
\code{\link{getYears}} to extract the set of available years\cr
\code{\link{getCountries}} to extract the set of available countries\cr
\code{\link{getRegions}} to extract the set of available regions\cr
}

\examples{
\dontrun{

## connect to EURO HFA-DB
## this is the default database and is loaded at startup
setHFADB("http://sic.hi.lt/DPS/ws/dps_ws.php")

## show available languages, regions, indicator groups
getLanguages()
getRegions()
getIndicatorGroups()

## show available countries for 'EUR' region
getCountries("EUR")

## show indicators belonging to group 4
getIndicators(4)

## find indicators related to 'food'
findIndicators("food")

## show definition of indicator '4010' and '4021'
getDefinitions(c("4010", "4021"))

## extract data for indicator '4010' and '4021' for Belgium
## by not specifying 'year', we extract data for all years
getData(c("4010", "4021"), country = "BEL")

}
}