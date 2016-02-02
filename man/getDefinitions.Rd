\name{getDefinitions}
\alias{getDefinitions}

\title{Get Definitions}

\description{
Extract the definitions of the indicators.
}

\usage{
getDefinitions(indicator, language = NULL)
}

\arguments{
  \item{indicator}{ID (character) of the indicator; one or more IDs are allowed.}
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with two columns: \code{Id} and \code{Title}.
}

\seealso{
\code{\link{getIndicatorGroups}} to extract the set of available indicator groups\cr
\code{\link{getIndicators}} to extract the set of available indicators\cr
\code{\link{findIndicators}} to find indicators based on a text string\cr
\code{\link{getLanguages}} to extract the set of available languages
}