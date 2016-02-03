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
  ## check arguments
  if (class(url) != "character")
    stop(sQuote("url"), " must be of class character")
  if (class(verbose) != "logical")
    stop(sQuote("verbose"), " must be of class logical")

  ## check if 'url' exists
  if (!url.exists(url))
    stop("provided URL does not exist")

  ## set .hfa url
  HFAset("url", url)

  ## update choices
  HFAset("lan", getLanguages()$Id)
  HFAset("reg", getRegions()$RegID)
  HFAset("grp", getIndicatorGroups()$Id)

  ## database string
  if (verbose) print(getDBVersion())
}

## check for internet access
internet <-
function() {
  !is.null(nslookup("r-project.org", error = FALSE))
}

## set default HFA database at startup
.onAttach <-
function(libname, pkgname) {
  packageStartupMessage("Connecting...")
  flush.console()

  if (internet()) {
    setHFADB("http://sic.hi.lt/DPS/ws/dps_ws.php", verbose = FALSE)
    packageStartupMessage(
      cat(paste(rep("\b", 12), collapse = "")),
      paste0("Connected to:\n", getDBVersion(), "\n"))

  } else {
    packageStartupMessage(
      cat(paste(rep("\b", 12), collapse = "")),
      "Internet access is required to use the HFA package.\n",
      "Use setHFADB() to initiate your HFA session.\n")
  }
}