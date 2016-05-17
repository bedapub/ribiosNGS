library(ribiosArg)

rexe <- file.path(dirname(dirname(commandArgs()[1])),
                  "R")
rscript <- system.file("Rscript/test-argParse.Rscript", package="ribiosArg")
comm1 <- sprintf("%s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b -e a", rscript)
comm2 <- sprintf("%s -f %s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b -e b", rexe, rscript)
system(comm1)
system(comm2)

ns.rscript <- system.file("Rscript/test-argParse-notstrict.Rscript", package="ribiosArg")
comm1 <- sprintf("%s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b -e c", rscript)
comm2 <- sprintf("%s -f %s -infile blablabla -outfile bla2 -a 34 48 -c 45 -b f1 f2 f3 -e b", rexe, rscript)
system(comm1)
system(comm2)
