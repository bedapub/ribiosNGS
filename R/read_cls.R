read_cls <- function (cls.file) {
    lns <- readLines(cls.file)
    stopifnot(length(lns) == 3)
    indims <- as.integer(strsplit(lns[1L], "\\s")[[1]])
    if (!(length(indims) == 3L & indims[3L] == 1L)) {
        stop("The first line of cls file is mal-formated.\n")
    }
    slevels <- strsplit(lns[2L], "\\s")[[1L]]
    if (!(identical(slevels[1], "#") & length(slevels) == indims[2L] + 
        1L)) {
        stop("The label line of cls file has fewer classes than indicated.\n")
    }
    slevels <- slevels[-1L]
    fn <- as.integer(strsplit(lns[3], "\\s")[[1]])
    if (length(fn) != indims[1L]) {
        stop("In cls file there are number-indexed samples than indicated in the header line.\n ")
    }
    sf <- factor(slevels[fn+1],
                 levels=slevels)
    return(sf)
}
