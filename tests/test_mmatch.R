library(ribiosUtils)

test <- mmatch(c("A", "B", "C", "T", "B", "S", "Z", NA, "C", NULL),
               c("A", "B","C","D", "A", "D", "B", "C", "C", "A"))
test2 <-  mmatch(c("A", "B", "C", "T", "B", "S", "Z", NA, "C", NULL),
                 c("A", "B","C","D", "A", "D", "B", "C", "C", "A"),
                 nomatch=0)

torSource <- sample(letters, 20000, replace=TRUE)
torTarget <- sample(letters, 20000, replace=TRUE)
system.time(torMatch <- match(torSource, torTarget))
system.time(torMultiMatch <- mmatch(torSource, torTarget))

namedSources <- c(a="txt", b="csv", c="pdf")
nmm <- mmatch(namedSources, c("csv", "pdf", "pdf", "txt", NA))
## all(sapply(1:20000, function(x) identical(unique(torTarget[torMultiMatch[[x]]]), torSource[[x]])))


