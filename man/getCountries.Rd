\name{getCountries}
\alias{getCountries}

\title{Get Countries}

\description{
Extract the countries for which data are available for a given region.
}

\usage{
getCountries(region, language = NULL)
}

\arguments{
  \item{region}{ID (character) of the region; only one ID is allowed.}
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with three columns: \code{RegID}, \code{CntrID}, and \code{Title}.
}

\seealso{
\code{\link{getRegions}} to extract the set of available regions\cr
\code{\link{getLanguages}} to extract the set of available languages
}