### HFA-DB - RCURL - version 2
### last update: 01/02/2016
### .. request fields derived from PHP implementation
### .. tag function deals with vectors
### .. lowerCamelCase for consistency with WSDL


##
## CONNECT TO WEB SERVICE
##

## define 'item' function
## .. creates tag for one element in SOAP array
item <-
function(x) {
  sprintf('<item xsi:type="xsd:string">%s</item>', x)
}

## define tag function
## .. creates tag for one argument
tag <-
function(arg) {
  ## extract argument value
  argValue <- unlist(arg)

  ## if arg is not specified, set to 'nil'
  if (is.null(argValue)) {
    sprintf('<%s xsi:nil="true"/>',
            names(arg))

  ## if arg consists of one element
  } else if (length(argValue) == 1) {
    sprintf('<%1$s xsi:type="xsd:string">%2$s</%1$s>',
            names(arg), argValue)

  ## if arg consists of multiple elements
  } else {
    paste0(
      sprintf('<%s SOAP-ENC:arrayType="xsd:string[%s]"
                   xsi:type="SOAP-ENC:Array">',
              names(arg), length(argValue)),
      paste(sapply(arg, item), collapse = "\n"),
      sprintf("</%s>", names(arg)),
      sep = "\n")
  }
}

## define request body
requestBody <-
function(f, ...) {
  ## collect arguments
  args <- list(...)

  ## parse arguments string
  if (length(args) > 0) {
    argString <- tapply(args, seq_along(args), tag)
    argString <- paste(argString, collapse = "\n")

  } else {
    argString <- NULL
  }

  ## paste raw request body
  paste0(
    sprintf(
      '<?xml version="1.0" encoding="UTF-8"?>
       <SOAP-ENV:Envelope
         xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
         xmlns:ns1="urn:xmethods-delayed-quotes"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xmlns:xsd="http://www.w3.org/2001/XMLSchema"
         xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
        SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
       <SOAP-ENV:Body>
        <ns1:%s>',
      f),
    argString,
    sprintf(
      ' </ns1:%s>
       </SOAP-ENV:Body>
       </SOAP-ENV:Envelope>',
      f))
}

## generic connect wrapper
connect <-
function(f, ...) {
  reader <- basicTextGatherer()
  curlPerform(url = getHFADB(),
              httpheader = c(Accept = "text/xml"),
              postfields = requestBody(f, ...),
              writefunction = reader$update)
  reader$value()
}


##
## EXTRACT RESPONSE
##

## check and convert vector to numeric
toNumeric <-
function(x) {
  ## check if NA introduced by 'as.numeric'
  ## .. convert to numeric if not
  if (!any(is.na(suppressWarnings(as.numeric(x))))) {
    as.numeric(x)

  ## .. else return original vector
  } else {
    x
  }
}

## convert NULL values in list to NA
null2na <-
function(x) {
  lapply(x, function(y) replace(y, is.null(y), NA))
}

## convert JSON to dataframe
json2dataframe <-
function(text, asNumeric) {
  ## JSON to LIST
  list <- fromJSON(text)

  ## LIST to DATAFRAME
  ## .. some functions return dataset with NULL values
  ## .. need to replace NULL values by NA values
  list <- lapply(list, null2na) 
  df <- as.data.frame(t(sapply(list, unlist)),
                      stringsAsFactors = FALSE)

  ## convert columns to numeric if applicable
  if (!is.null(asNumeric)) {
    df[, asNumeric] <-
      data.frame(lapply(df[asNumeric], toNumeric),
                 stringsAsFactors = FALSE)
  }

  ## return dataframe
  return(df)
}

## convert JSON to vector
json2vector <-
function(text) {
  ## JSON to VECTOR
  vector <- fromJSON(text)

  ## convert vector to numeric if applicable
  vector <- toNumeric(vector)

  ## return vector
  return(vector)
}

## generic extract wrapper
extract <-
function(xml, type = c("text", "vector", "dataframe"), asNumeric = NULL) {
  ## check 'type' input
  type <- match.arg(type)

  ## extract text value
  text <- xmlToList(xml)$Body[[1]]$return$text

  ## check for empty output
  if (text == "[]"){
    warning("No data found.", call. = FALSE)
    return(NULL)
  }

  ## define output
  out <-
  switch(type,
         text = text,
         vector = json2vector(text),
         dataframe = json2dataframe(text, asNumeric))

  ## return output
  return(out)
}
