queryUrl <- function(url, cookie) {
  if(missing(cookie) || is.null(cookie))
    stop('No authentication cookie was detected. Call \"bakeCookie\" to make one.')
  postForm(url,
           .opts=curlOptions(url=url,
             httpheader=paste("Cookie", cookie, sep=":")),
           style="post")
}

queryExpressionData <- function(studyid="NCS_tissue_rat",
                                cookie) {
  EXP.CGI <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi"
  queryCGI <- sprintf("%s?query=%s&studyidexternal=%s&studydomain=undefined&format=%s&outdest=browser",
                      EXP.CGI,
                      "signals", studyid, "gct")
  str <- queryUrl(queryCGI, cookie)
  if(nchar(str)>50 && !grepl("^#err", str)) {
    mat <- read_gctstr_matrix(str, keep.desc=TRUE)
    rm(str)
    return(mat)
  } else {
    cat(str)
    return(NULL)
  }
}
