queryUrl <- function(url) {
  cookie <- bioinfoCookie()
  if(missing(cookie) || is.null(cookie))
    stop('No authentication cookie was detected. Call \"bioinfoCookie()\" to make one.')
  postForm(url,
           .opts=curlOptions(url=url,
             httpheader=paste("Cookie", cookie, sep=":")),
           style="post")
}
