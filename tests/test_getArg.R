## test getArg
library(ribiosArg)

rscript <- file.path(dirname(dirname(commandArgs()[1])),
                     "Rscript")
if(file.exists(rscript)) {
  comm.format <- paste(rscript,
                       "-e \"",
                       "library(ribiosArg);",
                       "getArg(\\\"%s\\\", onlyArg=%s, missingArg=%s);",
                       "quit(status=0, save=\\\"no\\\")",
                       "\"",
                       "-infile a.file b.file -outfile o.file o2.file -value 2,3 -value2 3 4 6 NA -hey -hallo",
                       collapse=" ")
  comm <- sprintf(comm.format, "infile", "NA", "NA")
  comm.out <- system(comm, intern=TRUE)
  stopifnot(identical(comm.out, "[1] \"a.file\" \"b.file\""));
  
  comm2 <- sprintf(comm.format, "outfile", "NULL", "NULL")
  comm2.out <- system(comm2, intern=TRUE)
  stopifnot(identical(comm2.out,  "[1] \"o.file\"  \"o2.file\""))

  comm3 <- sprintf(comm.format, "value", "NULL", "NA")
  comm3.out <- system(comm3, intern=TRUE)
  stopifnot(identical(comm3.out, "[1] \"2,3\""))

  comm4 <- sprintf(comm.format, "value2", "\\\"0\\\"","\\\"0\\\"")
  comm4.out <- system(comm4, intern=TRUE)
  stopifnot(identical(comm4.out, "[1] \"3\"  \"4\"  \"6\"  \"NA\""))

  comm5 <- sprintf(comm.format, "hallo", "\\\"1887\\\"", "NULL")
  comm5.out <- system(comm5, intern=TRUE)
  stopifnot(identical(comm5.out,  "[1] \"1887\""))

  comm6 <- sprintf(comm.format, "hello", "\\\"Impossible\\\"", "NA")
  comm6.out <- system(comm6, intern=TRUE)
  stopifnot(identical(comm6.out, "[1] NA"))

  comm6a <- sprintf(comm.format, "hello", "\\\"Impossible\\\"", "\\\"Sure\\\"")
  comm6a.out <- system(comm6a, intern=TRUE)
  stopifnot(identical(comm6a.out, "[1] \"Sure\""))

  comm6b <- sprintf(comm.format, "hello", "\\\"Impossible\\\"", "NULL")
  comm6b.out <- system(comm6b, intern=TRUE)
  stopifnot(identical(comm6b.out, "NULL"))
}
