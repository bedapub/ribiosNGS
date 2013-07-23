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
cat("<div>Input parameters:<ol>")
for(i in seq(along=pars))
  cat("<li>", names(pars)[i], "=", pars[i], "</li>")
cat("</ol><div>")

## parse CGI parameter and make plot
title="Try appending '?main=A plot' to the URL"
if("main" %in% names(pars)) title <- pars["main"]
lty <- 1
if("lty" %in% names(pars)) lty <- as.integer(pars["lty"])

png("/DATA/bi/httpd_8080/htdoc/sawitmp/test3.png", type="cairo")
curve(log10, main=title, lty=lty)
invisible(dev.off())
cat("<div><img src='http://bioinfo.bas.roche.com:8080/sawitmp/test3.png'/></div>")
invisible(system("echo \"rm -f /DATA/bi/httpd_8080/htdoc/sawitmp/test3.png\" | at now + 5 minutes ", intern=TRUE))
