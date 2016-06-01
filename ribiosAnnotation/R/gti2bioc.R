## translate GTI chiptype to bioc chiptype
#' @export bioc2gti
bioc2gti <- function (chipname) {
  data("gtibioc", package="ribiosAnnotation")
  if(missing(chipname)) {

    supp <- !is.na(gtibioc$Bioconductor)
    vec <- gtibioc$GTI[supp]
    names(vec) <- gtibioc$Bioconductor[supp]
    return(vec)
  } else {
    matchColumn(chipname, gtibioc, "Bioconductor")$GTI
  }
}

#' @export gti2bioc
gti2bioc <- function(chipname) {
  data("gtibioc", package="ribiosAnnotation")
  if(missing(chipname)) {
    supp <- !is.na(gtibioc$Bioconductor)
    vec <- gtibioc$Bioconductor[supp]
    names(vec) <- gtibioc$GTI[supp]
    return(vec)
  } else {
    matchColumn(chipname, gtibioc, "GTI")$Bioconductor
  }
}
