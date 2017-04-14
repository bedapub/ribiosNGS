library(ribiosAnnotation)

hc <- chrLen("human")
mc <- chrLen("mouse")
rc <- chrLen("rat")

stopifnot(all(c(1:22, "X", "Y", "M") %in% hc$Chromosome))
