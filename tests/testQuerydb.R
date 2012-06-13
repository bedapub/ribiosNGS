library(ribiosAnnotation)

hc <- querydb("SELECT * FROM genome_sequence WHERE DB='HUMANN'", db="bin", user="genome", password="genome")
stopifnot(all(paste("CHR", 1:22, sep="") %in% hc$SEQ))
