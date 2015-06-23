#!/bin/tcsh /SOFT/bi/apps/R/bdeRscript

## test getArg
library(ribiosArg)
print(scriptName())
print(commandArgs())
stopifnot(identical("test_scriptName.R", scriptName()))

