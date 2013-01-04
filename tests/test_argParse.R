library(ribiosArg)

rexe <- file.path(dirname(dirname(commandArgs()[1])),
                  "R")
rscript <- system.file("Rscript/test-argParse.Rscript", package="ribiosArg")
comm1 <- sprintf("%s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b", rscript)
comm2 <- sprintf("%s -f %s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b", rexe, rscript)
system(comm1)
system(comm2)
