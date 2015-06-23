#!/bin/tcsh /SOFT/bi/apps/R/bdeRscript

library(ribiosCGI)

cgiInit()
cgiGet2Post()
cgiHeader("text/html")

## Basic text rendering
cat("<h1>Hello world!</h1>The webpage is rendered by R and ribiosCGI at",
    format(Sys.time(), "%Y-%M-%d %H:%M:%S"),
    "</div>")

## show CGI parameters
pars <- cgiParams()
cat("<div>All input parameters:<ol>")
for(i in seq(along=pars))
  cat("<li>", names(pars)[i], "=", pars[i], "</li>")
cat("</ol></div>")

cat("<div>Query specific parameters:<ol>")
cat("<li> main=", cgiParam("main", ignore.case=FALSE, default="not specified"), "</li>");
cat("<li> xLaB=", cgiParam("xlab", ignore.case=TRUE, default=NULL), "</li>");
cat("<li> yLaB=", cgiParam("ylab", ignore.case=TRUE, default=NULL), "</li>");
cat("<li> pairs=");
print(cgiPairParam("pairs"))
cat("</li>")
cat("</ol>")

## parse CGI parameter and make plot
title="Try appending '?main=A plot' to the URL"
if("main" %in% names(pars)) title <- pars["main"]
lty <- 1
if("lty" %in% names(pars)) lty <- as.integer(pars["lty"])

tmp <- tmpWebFile(pattern="file", fileext=".png")
tmpurl <- tmpWebURL(tmp)

openFileDevice(tmp, dpi=200, width=4L, height=4L)
curve(log10, main=title, lty=lty)
closeFileDevice()
cat("<div><img src='", tmpurl, "' style='width:600px;'/></div>", sep="")
cat("<div class=\"system\" style=\"visibility:none;\">")
rmat(tmp, minutes=1, dry=FALSE)
cat("</div>")
cat("Value of cgiIsCGI():", cgiIsCGI())
