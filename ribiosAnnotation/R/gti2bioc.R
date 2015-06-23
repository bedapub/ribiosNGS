## translate GTI chiptype to bioc chiptype
bioc2gti <- function (chipname) 
{
  if(missing(chipname)) {
    supp <- !is.na(gtibioc$Bioconductor)
    vec <- gtibioc$GTI[supp]
    names(vec) <- gtibioc$Bioconductor[supp]
    return(vec)
  } else {
    matchColumn(chipname, gtibioc, "Bioconductor")$GTI
  }
}

gti2bioc <- function(chipname) {
  if(missing(chipname)) {
    supp <- !is.na(gtibioc$Bioconductor)
    vec <- gtibioc$Bioconductor[supp]
    names(vec) <- gtibioc$GTI[supp]
    return(vec)
  } else {
    matchColumn(chipname, gtibioc, "GTI")$Bioconductor
  }
}
