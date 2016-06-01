

##IDENT.POST.URL <- "http://bioinfo.bas.roche.com:8080/htbin/identwwwbicgi?application"
##
##bakeCookie <- function(appname="udis_expression",
##                       user,
##                       apppassword,
##                       verbose=TRUE) {
##  ## hg and tg parse header and body respectively
##  hg <- basicHeaderGatherer()
##  tg <- basicTextGatherer()
##
##  ## note that style must be set to post since this is application/x-www-form-urlencoded
##  h <- postForm(IDENT.POST.URL,
##                .opts=curlOptions(url=IDENT.POST.URL,
##                  headerfunction=hg$update,
##                  writefunction=tg$update),
##                .params=c(appname=appname,
##                  user=user,
##                  apppassword=apppassword),
##                style="post")
##  msg <- tg$value()
##  if(verbose)
##    cat(msg, "\n")
##  
##  hfields <- hg$value()  
##  if("Set-Cookie" %in% names(hfields)) {
##    return(hfields[["Set-Cookie"]])
##  } else {
##    warning("No 'Set-Cookie' field in the returning header. Cookie baking failed")
##    return(NULL)
##  }
##}
