## translate GTI chiptype to bioc chiptype
gti2bioc <- function(chipname) {
  matchColumn(chipname, gtibioc, "GTI")$Bioconductor
}

bioc2gti <- function(chipname) {
  matchColumn(chipname, gtibioc, "Bioconductor")$GTI
}
