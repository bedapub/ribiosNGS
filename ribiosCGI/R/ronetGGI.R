paste0 <- function(...) paste(..., sep="", colapse=NULL)
cgiEncode <- function(strVec)  paste(sapply(strVec, cgiEncodeWord), collapse="+")

#' Query gene-gene interactions (GGI) in RONET
#' @param geneids A vector of Entrez GeneIDs
#' @param mode Interaction mode
#' @param intTypes Interaction types allowed
#' @param sources Sources allowed
#' @examples 
#' \dontrun{
#'   ## query interactions between UCP1 and UCP2
#'   ronetGGI(c(7350, 7351), mode="between")
#' }
#' 
#' @export
#' @importFrom ribiosUDIS queryUrl
ronetGGI <- function(geneids,
                       mode=c("in", "out", "inout", "between"),
                       intTypes=NULL,
                       sources=NULL) {
    mode <- match.arg(mode)
    url <- paste0("http://bioinfo.bas.roche.com:8080/bicgi/ronet_query_cgi",
                 "?ids=", paste(geneids, collapse="+"), "&",
                 "mode=", mode,"&",
                 "format=tab")
    if(!is.null(intTypes))
        url <- paste0(url,
                     "&intType=", cgiEncode(intTypes))
    if(!is.null(sources))
        url <- paste0(url,
                      "&sources=", cgiEncode(sources))
    txt <- queryUrl(url)
    res <- read.table(textConnection(txt), sep="\t", comment="", head=TRUE)
    return(res)
    
}
