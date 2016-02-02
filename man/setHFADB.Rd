\name{setHFADB}
\alias{setHFADB}

\title{Set HFA database URL}

\description{
Set the URL of the HFA database.
}

\usage{
setHFADB(url, verbose = TRUE)
}

\arguments{
  \item{url}{URL (character) of the HFA database.}
  \item{verbose}{If \code{TRUE}, the current version of the HFA database is printed.}
}

\author{\email{brechtdv@gmail.com}}

\seealso{
\code{\link{getHFADB}} to get the current HFA database URL\cr
\code{\link{getDBVersion}} to get the current version of the HFA database
}

\examples{
## connect to EURO HFA-DB
## this is the default database and is loaded at startup
setHFADB("http://sic.hi.lt/DPS/ws/dps_ws.php")
}