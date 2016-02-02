\name{getRegions}
\alias{getRegions}

\title{Get Regions}

\description{
Extract the regions for which data are available.
}

\usage{
getRegions(language = NULL)
}

\arguments{
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with three columns: \code{RegID}, \code{RegPID}, and \code{Title}.
}

\seealso{\code{\link{getLanguages}} to extract the set of available languages}