GeMS_BASE_URL <- "http://bioinfo.bas.roche.com:1234/api"


GeMS_INSERT_URL <- paste(GeMS_BASE_URL, "/insert", sep="")
GeMS_REMOVE_URL <- paste(GeMS_BASE_URL, "/remove", sep="")
GeMS_GENESETS_URL <- paste(GeMS_BASE_URL, "/genesets", sep="")


#' Test whether GeMS is reachable
#' @return Logical value
#' @examples 
#' \dontrun{
#' isGeMSReachble()
#' }
isGeMSReachable <- function() {
   !http_error(GET(GeMS_GENESETS_URL)) 
}

#' Send a list as JSON query to an URL and fetch the response
#' 
#' @param url The destination URL
#' @param body A list to be sent to the URL, which will be encoded in the JSON format internally
#' 
#' @return The response from the webserver
#' 
#' @examples 
#' \dontrun{
#'    getJsonResponse(GeMS_GENESETS_URL, list(user=ribiosUtils::whoami()))
#' }
getJsonResponse <- function(url, body) {
  response <- httr::POST(url, body=body, encode='json')
  returnJSON <- jsonlite::fromJSON(httr::content(response, 'text', encoding='UTF-8'))
  return(returnJSON$response)
}

#' Construct message body to insert into GeMS
#' @param gmtList A \code{GmtList} object defined in the \code{BioQC} package
#' @param geneFormat Integer index of gene format. 0 stands for official human gene symbol
#' @param source Character, source of the gene set
#' @param xref External reference, will be written in the meta field of the backend database
#' @param taxID Integer, NCBI taxonomy ID of the species.
#' @param user The user name
#' 
#' @return A list with three items: \code{headers}, \code{parsed}, and \code{params}
#' 
#' @examples 
#' testList <- list(list(name="GS_A", desc=NULL, genes=c("MAPK14", "JAK1", "EGFR")),
#'   list(name="GS_B", desc="gene set B", genes=c("ABCA1", "DDR1", "DDR2")),
#'   list(name="GS_C", desc="gene set C", genes=NULL))
#' testGmt <- BioQC::GmtList(testList)
#' insertGmtListToGeMSBody(testGmt, geneFormat=0, source="Test", xref="PMID:000000")
insertGmtListToGeMSBody <- function(gmtList,
                                    geneFormat=0,
                                    source="PubMed",
                                    xref="",
                                    taxID=9606,
                                    user=ribiosUtils::whoami()) {
  parsed <- lapply(gmtList, function(x) unname(unlist(x[c("name", "desc", "genes")])))
  gemsPars <- list(gf=geneFormat,
                   so=source,
                   meta=list(xref=xref),
                   ti=taxID,
                   us=user)
  dataList <- list(headers=c("setName", "desc", "genes"),
                   parsed=parsed,
                   params=gemsPars)
  return(dataList)
}

#' Insert a GmtList object to GeMS
#' @param gmtList A \code{GmtList} object defined in the \code{BioQC} package
#' @param geneFormat Integer index of gene format. 0 stands for official human gene symbol
#' @param source Character, source of the gene set
#' @param xref External reference, will be written in the meta field of the backend database
#' @param taxID Integer, NCBI taxonomy ID of the species.
#' @param user The user name
#' 
#' @return Response code or error message returned by the GeMS API. A value of \code{200} indicates a successful insertion.
#' 
#' @seealso \code{\link{removeFromGeMS}}
#' @examples 
#' \dontrun{
#'   testList <- list(list(name="GS_A", desc=NULL, genes=c("MAPK14", "JAK1", "EGFR")),
#'     list(name="GS_B", desc="gene set B", genes=c("ABCA1", "DDR1", "DDR2")),
#'     list(name="GS_C", desc="gene set C", genes=NULL))
#'   testGmt <- BioQC::GmtList(testList)
#'   insertGmtListToGeMS(testGmt, geneFormat=0, source="Test", xref="PMID:000000")
#'   removeFromGeMS(setName=c("GS_A", "GS_B", "GS_C"), source="Test")
#' }
insertGmtListToGeMS <- function(gmtList,
                                geneFormat=0,
                                source="PubMed",
                                xref="",
                                taxID=9606,
                                user=ribiosUtils::whoami()) {
  dataList <- insertGmtListToGeMSBody(gmtList=gmtList,
                                  geneFormat=geneFormat,
                                  source=source,
                                  xref=xref,
                                  taxID=taxID,
                                  user=user)
  return(getJsonResponse(GeMS_INSERT_URL, dataList))
}

#' Message body to remove one or gene sets of the same source and user from GeMS
#' 
#' @param setName A vector of character strings, defining set names to be renamed. They must all have the same \code{source}, \code{user}, and \code{subtype}
#' @param source Character string, source of the gene set(s)
#' @param user Character string, user name
#' @param subtype Character string, subtype of the gene set(s)
#' 
#' @return A list of genesets to be removed, to be sent as message body
#' 
#' @examples 
#' removeFromGeMSBody(setName=c("GS_A", "GS_B", "GS_C"), source="Test")
removeFromGeMSBody <- function(setName="", source="", user=ribiosUtils::whoami(), subtype="") {
  toRemove <- lapply(setName, function(sname) {
    list(setName=sname,
         source=source,
         user=user,
         subtype=subtype)
  })
  res <- list(genesets=toRemove)
  return(res)
}

#' Rmove one or gene sets of the same source and user from GeMS
#' @param setName A vector of character strings, defining set names to be renamed. They must all have the same \code{source}, \code{user}, and \code{subtype}
#' @param source Character string, source of the gene set(s)
#' @param user Character string, user name
#' @param subtype Character string, subtype of the gene set(s)
#' 
#' @return Response code or error message returned by the GeMS API. A value of \code{200} indicates a successful insertion.
#' 
#' @seealso \code{\link{insertGmtListToGeMS}}
#' @examples 
#' \dontrun{
#'   testList <- list(list(name="GS_A", desc=NULL, genes=c("MAPK14", "JAK1", "EGFR")),
#'     list(name="GS_B", desc="gene set B", genes=c("ABCA1", "DDR1", "DDR2")),
#'     list(name="GS_C", desc="gene set C", genes=NULL))
#'   testGmt <- BioQC::GmtList(testList)
#'   insertGmtListToGeMS(testGmt, geneFormat=0, source="Test", xref="PMID:000000")
#'   removeFromGeMS(setName=c("GS_A", "GS_B", "GS_C"), source="Test")
#' }
#' 
removeFromGeMS <- function(setName="", source="", user=ribiosUtils::whoami(), subtype="") {
  body <- removeFromGeMSBody(setName, source, user, subtype)
  return(getJsonResponse(GeMS_REMOVE_URL, body))
}

#' Get gene sets of a user from GeMS
#' @param user User name
#' @return A data.frame including following columns: 
#' \enumerate{
#'   \item setName
#'   \item desc
#'   \item domain
#'   \item source
#'   \item subtype
#' }
#' 
#' @examples 
#' \dontrun{
#' ## my gene-sets
#' getUserSetsFromGeMS()
#' ## from another user
#' getUserSetsFromGeMS("kanga6")
#' }
getUserSetsFromGeMS <- function(user=ribiosUtils::whoami()) {
  fieldsOfInterest <- c("setName", "desc", "domain",
                         "source", "subtype")
  body <- list(user=user,
               returnParams=as.list(fieldsOfInterest))
  df <- getJsonResponse(GeMS_GENESETS_URL, body)
  if(is.list(df) && length(df)==0) {
    res <- NULL
  } else {
    res <- df[, fieldsOfInterest]
  }
  return(res)
}

