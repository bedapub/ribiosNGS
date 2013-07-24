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
cat("</ol>")

## parse CGI parameter and make plot
title="Try appending '?main=A plot' to the URL"
if("main" %in% names(pars)) title <- pars["main"]
lty <- 1
if("lty" %in% names(pars)) lty <- as.integer(pars["lty"])

tmp <- tmpWebFile(pattern="file", fileext=".png")
tmpurl <- tmpWebURL(tmp)

png(tmp, type="cairo")
curve(log10, main=title, lty=lty)
invisible(dev.off())
cat("<div><img src='", tmpurl, "'/></div>", sep="")
cat("<div class=\"system\" style=\"visibility:none;\">")
invisible(system("echo \"rm -f /DATA/bi/httpd_8080/htdoc/sawitmp/test3.png\" | at now + 5 minutes ", intern=TRUE))
cat("</div>")
cat("Value of cgiIsCGI():", cgiIsCGI())
