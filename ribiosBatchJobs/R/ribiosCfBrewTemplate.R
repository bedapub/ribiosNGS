#' cfBrewTemplate customed for ribios
#'
#' @param conf the same as in cfBrewTemplate
#' @param template the same as in cfBrewTemplate
#' @param rscript the same as in cfBrewTemplate
#' @param extension the same as in cfBrewTemplate
#'
#' The function is adapted from cfBrewTemplate. Instead of writing to /tmp/, which can be only accessed by the local machine but not the remote machine, the function uses ribiosTempfile implemented in ribiosUtils to allow cluster-visible temporary files.
#' @export
#' @import BatchJobs
#' @importFrom checkmate assertEnvironment assertString
#' @importFrom brew brew
#' @importFrom ribiosUtils ribiosTempfile
#' @importFrom BBmisc suppressAll is.error stopf
ribiosCfBrewTemplate <- function(conf, template, rscript, extension) {
    checkmate::assertEnvironment(conf)
    checkmate::assertString(template)
    checkmate::assertString(rscript)
    checkmate::assertString(extension)
    if (conf$debug) {
        outfile = sub("\\.R$", sprintf(".%s", extension), rscript)
    }
    else {
        outfile = ribiosUtils::ribiosTempfile("template")
    }
    pf = parent.frame()
    old = getOption("show.error.messages")
    on.exit(options(show.error.messages = old))
    options(show.error.messages = FALSE)
    z = suppressAll(try(brew::brew(text = template, output = outfile, 
        envir = pf), silent = TRUE))
    if (is.error(z)) 
        stopf("Error brewing template: %s", as.character(z))
    BatchJobs:::waitForFiles(outfile, conf$fs.timeout)
    return(outfile)
}
