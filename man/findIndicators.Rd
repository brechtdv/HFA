\name{findIndicators}
\alias{findIndicators}

\title{Find Indicators}

\description{
Find indicators based on a given text string.
}

\usage{
findIndicators(text, language = NULL)
}

\arguments{
  \item{text}{Text string (character) that will be matched; only one text string is allowed.}
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with two columns: \code{Id} and \code{Title}.
}

\seealso{
\code{\link{getIndicatorGroups}} to extract the set of available indicator groups\cr
\code{\link{getIndicators}} to extract the set of available indicators\cr
\code{\link{getDefinitions}} to extract the definitions of the indicators\cr
\code{\link{getLanguages}} to extract the set of available languages
}