\name{HFA-package}
\alias{HFA-package}
\alias{HFA}

\docType{package}

\title{R interface to European Health For All Database (HFA-DB)}

\description{R interface to European Health For All database (HFA-DB). For more information on HFA-DB, please visit \url{http://www.euro.who.int/en/data-and-evidence/databases/european-health-for-all-database-hfa-db}.}

\details{
  \tabular{ll}{
    Package:    \tab  HFA\cr
    Type:       \tab  Package\cr
    Version:    \tab  0.0.2\cr
    Date:       \tab  2016-02-01\cr
    Authors:    \tab  Brecht Devleesschauwer\cr
    Maintainer: \tab  \email{brechtdv@gmail.com}\cr
    Depends:    \tab  R (>= 3.2.0)\cr
    Imports:    \tab  RCurl, rjson, XML\cr
    License:    \tab  GPL (>= 2)\cr
    LazyLoad:   \tab  yes\cr
}

Available functions in the \pkg{HFA} package:
  \tabular{ll}{
    \code{\link{getDBVersion}}        \tab Print current version of the HFA-DB.\cr
    \code{\link{getLanguages}}        \tab Extract available languages.\cr
    \code{\link{getRegions}}          \tab Extract available regions.\cr
    \code{\link{getCountries}}        \tab Extract available countries for a given region.\cr
    \code{\link{getYears}}            \tab Extract available years for a given country.\cr
    \code{\link{getIndicatorGroups}}  \tab Extract set of indicator groups.\cr
    \code{\link{getIndicators}}       \tab Extract set of indicators for a given indicator group.\cr
    \code{\link{getDefinitions}}      \tab Extract definitions for a given set of indicators.\cr
    \code{\link{findIndicators}}      \tab Find indicators containing specific text.\cr
    \code{\link{getData}}             \tab Extract data by indicator, year, country and region.\cr
  }
}

\keyword{package}
