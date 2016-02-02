\name{getIndicators}
\alias{getIndicators}

\title{Get Indicators}

\description{
Extract the indicators for which data are available for a given indicator group.
}

\usage{
getIndicators(group, language = NULL)
}

\arguments{
  \item{group}{ID (numeric or character) of the indicator group; only one ID is allowed.}
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with three columns: \code{IndID}, \code{IndPID}, and \code{Title}.
}

\seealso{
\code{\link{getIndicatorGroups}} to extract the set of available indicator groups\cr
\code{\link{findIndicators}} to find indicators based on a text string\cr
\code{\link{getDefinitions}} to extract the definitions of the indicators\cr
\code{\link{getLanguages}} to extract the set of available languages
}