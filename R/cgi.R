cgiIsCGI <- function() {
  .Call("r_cgiIsCGI")
}

cgiInit <- function() {
  invisible(.Call("r_cgiInit"))
}

cgiGet2Post <- function() {
  invisible(.Call("r_cgiGet2Post"))
}

cgiGet2PostReset <- function() {
  invisible(.Call("r_cgiGet2PostReset"))
}

cgiHeader <- function(header) {
  invisible(.Call("r_cgiHeader",
                  as.character(header)))
}

cgiParams <- function() {
  return(.Call("r_cgiParameters"))
}

cgiParam <- function(name, ignore.case=FALSE, default=NULL) {
  return(.Call("r_cgiParam", as.character(name), as.logical(ignore.case), default))
}

## Following functions are based on cgiParam
## cgiNumParam: parse numeric CGI paramters
cgiNumParam <- function(name, ignore.case=FALSE, expLen=NULL, default=NULL) {
  x <- cgiParam(name,
                ignore.case=ignore.case, default=NULL)
  if(is.null(x)) return(default)
  x <- strsplit(x, ",| ")[[1]] ## allow comma-sep or plus-sep values
  x <- x[ x != "" ]

  xnum <- suppressWarnings(as.numeric(x))
  isNum <- all(!is.na(xnum))
  if(!is.null(expLen))
    isNum <- isNum && length(x) == expLen
  if(isNum)
    return(xnum)
  else
    return(default)
}

## cgiCateParam/cgiEnumParam: parse categorical variable CGI parameters
cgiCateParam <- function(name, ignore.case=FALSE,
                         allowed.values=NULL, default=NULL) {
  haltifnot(length(allowed.values)>=1,
            msg="Empty allowed.values is not allowed")
  haltifnot(is.null(default) || default %in% allowed.values,
            msg="The default value must be either NULL or in the allowed.values")
  x <- cgiParam(name, ignore.case=ignore.case, default=default)
  haltifnot(is.null(x) || x %in% allowed.values,
            msg=paste("Allowed values of '", name, "' include\n",
              paste(allowed.values, collapse=", "),
              ".\n The input value '", x ,"' was invalid", sep=""))
  return(x)
}
cgiEnumParam <- cgiCateParam

## cgiColParam: parse color-name CGI parameters
cgiColParam <- function(name, ignore.case=FALSE, default=1L) {
  x <- cgiParam(name, ignore.case=ignore.case, default=NULL)
  if(is.null(x)) return(default)
  x <- gsub("\"", "", x)
  x <- strsplit(x, ",| ")[[1]]
  x <- x[x!=""]
  if(length(x)==0) return(default)
  if(all(grepl("^[0-9]*$", x)))
    return(as.integer(x))
  return(x)
}

## cgiLogiParam: parse logical CGI parameters
cgiLogiParam <- function(name, ignore.case=FALSE, default=FALSE) {
  haltifnot(is.logical(default), msg="Default must be a logical value")
  x <- cgiParam(name, ignore.case=ignore.case, default=default)
  xl <- as.logical(x)
  if(!is.na(xl))  return(xl)
  return(default)
}

## cgiFunParam: parse CGI parameters for R function
cgiFuncParam <- function(name, ignore.case=FALSE, default=NULL) {
  x <- cgiParam(name, ignore.case=ignore.case, default=NULL)
  if(is.null(x)) return(default)
  if(grepl("function", x)) { ## a user defined function
    fun <- eval(parse(text=x))
  } else {
    fun <- getFunction(x, mustFind=TRUE)
  }
  attr(fun, "label") <- x
  return(fun)
}
