library(ribiosIO)

mySeqs <- c("sample1 motif"="ATGCGTG",
            "sample2 motif"="GTGCGTA",
            "sample3 motif"="GCGTGGA")
tmpfile <- tempfile()

write_fasta(mySeqs, tmpfile)

myReadinSeqs <- read_fasta(tmpfile)

stopifnot(identical(myReadinSeqs, mySeqs))
