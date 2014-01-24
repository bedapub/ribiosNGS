library(makecdfenv)

##----------------------------------------##
## URL for downloading the CDF file
##----------------------------------------##
URL <- "http://www.ncbi.nlm.nih.gov/geo/download/?acc=GPL16569&format=file&file=GPL16569%5FSNOWBALL%5Fgenelevel%2Ecdf%2Egz"

make.cdf.package(url(URL),
        packagename = "snowballs520824fcdf",
        cdf.path = getwd(),
        package.path = getwd(),
        compress = FALSE,
        author = "UDIS",
        maintainer = "Laura Badi <laura.badi@roche.com>",
        version = packageDescription("makecdfenv", fields ="Version"),
        species = "Sus scrofa",
        unlink = TRUE,
        verbose = TRUE)
