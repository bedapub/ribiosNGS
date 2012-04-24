queryUrl <- function(url) {
  cookie <- bioinfoCookie()
  if(missing(cookie) || is.null(cookie))
    stop('No authentication cookie was detected. Call \"bakeCookie\" to make one.')
  postForm(url,
           .opts=curlOptions(url=url,
             httpheader=paste("Cookie", cookie, sep=":")),
           style="post")
}

queryExpressionData <- function(studyid="NCS_tissue_rat") {
  EXP.CGI <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi"
  queryCGI <- sprintf("%s?query=%s&studyidexternal=%s&studydomain=undefined&format=%s&outdest=browser",
                      EXP.CGI,
                      "signals", studyid, "gct")
  str <- queryUrl(queryCGI)
  if(nchar(str)>50 && !grepl("^#err", str) && !grepl("^http", str)) {
    mat <- read_gctstr_matrix(str, keep.desc=TRUE)
    rm(str)
    return(mat)
  } else {
    cat(str)
    cat("\nConnection string:", queryCGI, "\n")
    return(NULL)
  }
}
