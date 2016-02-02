### HFA-DB - RCURL
### last update: 01/02/2016
### .. hfa environment and options

## create HFA environment
.hfa <- new.env()

## return HFA environment
HFAenv <- function() .hfa

## assign object to .hfa
HFAset <-
function(name, val) {
  assign(name, val, envir = HFAenv())
}

## get object from .hfa
HFAget <-
function(name) {
  get(name, envir = HFAenv())
}

## get HFA database
getHFADB <-
function() {
  HFAget("url")
}

## set HFA database
setHFADB <-
function(url, verbose = TRUE) {
  ## set .hfa url
  HFAset("url", url)

  ## update choices
  HFAset("lan", getLanguages()$Id)
  HFAset("reg", getRegions()$RegID)
  HFAset("grp", getIndicatorGroups()$Id)

  ## database string
  if (verbose) print(getDBVersion())
}

## set default HFA database at startup
.onAttach <-
function(libname, pkgname) {
  setHFADB("http://sic.hi.lt/DPS/ws/dps_ws.php", verbose = FALSE)
  packageStartupMessage(paste0("\n", getDBVersion(), "\n"))
}