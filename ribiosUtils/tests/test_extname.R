library(ribiosUtils)

stopifnot(identical(extname("mybook.pdf"), "pdf"))
stopifnot(identical(extname("sequence.in.fasta"), "fasta"))
stopifnot(identical(extname(c("/path/mybook.pdf", "test.doc")),
                    c("pdf", "doc")))
                            
stopifnot(identical(extname("README"), NA))
stopifnot(identical(extname("/path/my\ home/Holiday Plan.txt"), "txt"))
