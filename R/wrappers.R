### HFA-DB - RCURL
### last update: 01/02/2016
### .. wrapper functions

getDBVersion <-
function() {
  ## .. args   NULL
  ## .. output text

  ## send request to WSDL
  con <- connect("getDBVersion")

  ## extract response
  out <- extract(con, "text")

  ## return output
  return(out)
}

getLanguages <-
function() {
  ## .. args   NULL
  ## .. output dataframe

  ## send request to WSDL
  con <- connect("getLanguagesJSON")

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getRegions <-
function(language) {
  ## .. args   'lang' as string
  ## .. output dataframe

  ## check arguments
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("getRegionsJSON", lang = language)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getCountries <-
function(region, language) {
  ## .. args   'lang' as string, 'reg' as string
  ## .. output dataframe

  ## check arguments
  region   <- match.arg(region, HFAget("reg"))
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("getCountriesJSON", lang = language, reg = region)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getYears <-
function(country = NULL) {
  ## .. args   'cntr' as string
  ## .. output vector

  ## check arguments
  if (length(country) > 1)
    stop(sQuote("country"), " should be either NULL or of length 1")

  ## send request to WSDL
  con <- connect("getYearsJSON", cntr = country)

  ## extract response
  out <- extract(con, "vector")

  ## return output
  return(out)
}

getIndicatorGroups <-
function(language) {
  ## .. args   'lang' as string
  ## .. output dataframe

  ## check arguments
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("getIndicatorGroupsJSON", lang = language)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getIndicators <-
function(group, language) {
  ## .. args   'lang' as string, 'grp' as string
  ## .. output dataframe

  ## check arguments
  if (length(group) > 1)
    stop(sQuote("group"), " should be of length 1")
  if (is.numeric(group))
    group <- as.character(group)

  group    <- match.arg(group, HFAget("grp"))
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("getIndicatorsJSON", lang = language, grp = group)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getDefinitions <-
function(indicator, language) {
  ## .. args   'lang' as string, 'defs' as ArrayOfString
  ## .. output dataframe

  ## check arguments
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("getDefinitionsJSON", lang = language, defs = indicator)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

findIndicators <-
function(text, language) {
  ## .. args   'lang' as string, 'txt' as string
  ## .. output dataframe

  ## check arguments
  if (length(text) > 1)
    stop(sQuote("text"), " should be of length 1")
  language <- match.arg(language, HFAget("lan"))

  ## send request to WSDL
  con <- connect("findIndicatorsJSON", lang = language, txt = text)

  ## extract response
  out <- extract(con, "dataframe")

  ## return output
  return(out)
}

getData <-
function(indicator, year = NULL, country = NULL, region = NULL) {
  ## .. args   'yrs' as ArrayOfInteger, 'inds' as ArrayOfString
  ## ..        'reg' as String, 'cntrs' as ArrayOfString
  ## .. output dataframe

  ## check arguments
  if (missing(indicator))
    stop(sQuote("indicator"), " is missing, with no default.\n",
         "See '?getIndicators' and '?findIndicators' for indicator codes.")

  if (!is.null(region))
    region <- match.arg("region", HFAget("reg"), several.ok = TRUE)

  ## send request to WSDL
  con <-
    connect("getDataJSON",
            yrs = year,
            inds = indicator,
            reg = region,
            cntrs = country)

  ## extract response
  out <- extract(con, "dataframe", asNumeric = c(1, 5))

  ## return output
  return(out)
}