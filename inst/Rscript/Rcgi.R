#!/bin/tcsh /SOFT/bi/apps/R/bdeRscript

library(ribiosCGI)

cgiInit()
cgiGet2Post()
cgiHeader("text/html")

## Basic text rendering
cat("Hello world!The webpage is rendered by R and ribiosCGI at",
    format(Sys.time(), "%Y-%M-%d %H:%M:%S"),
    "</div>")

## show CGI parameters
pars <- cgiParams()
cat("<div>Input parameters:<ol>")
for(i in seq(along=pars))
  cat("<li>", names(pars)[i], "=", pars[i], "</li>")
cat("</ol><div>")

## parse CGI parameter and make plot
title <- ifelse(is.na(pars["main"]), NULL, pars["main"])

png("/DATA/bi/httpd_8080/htdoc/sawitmp/test3.png", type="cairo")
curve(log10, main=title)
invisible(dev.off())
cat("<img src='http://bioinfo.bas.roche.com:8080/sawitmp/test3.png'/>")
invisible(system("echo \"rm -f /DATA/bi/httpd_8080/htdoc/sawitmp/test3.png\" | at now + 5 minutes ", intern=TRUE))
