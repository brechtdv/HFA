\name{getIndicatorGroups}
\alias{getIndicatorGroups}

\title{Get Indicator Groups}

\description{
Extract the available indicator groups.
}

\usage{
getIndicatorGroups(language = NULL)
}

\arguments{
  \item{language}{ID (character) of the desired language for output; only one ID is allowed. The default value, \code{NULL}, leads to the use of the first available language.}
}

\author{\email{brechtdv@gmail.com}}

\value{
Dataframe with two columns: \code{Id} and \code{Title}.
}

\seealso{
\code{\link{getIndicators}} to extract the set of available indicators\cr
\code{\link{findIndicators}} to find indicators based on a text string\cr
\code{\link{getDefinitions}} to extract the definitions of the indicators\cr
\code{\link{getLanguages}} to extract the set of available languages
}