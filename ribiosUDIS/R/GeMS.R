GeMS_BASE_URL <- "http://bioinfo.bas.roche.com:1234/api"

GeMS_INSERT_URL <- paste(GeMS_BASE_URL, "/insert", sep="")
GeMS_REMOVE_URL <- paste(GeMS_BASE_URL, "/remove", sep="")
GeMS_GENESETS_URL <- paste(GeMS_BASE_URL, "/genesets", sep="")

#' System user name
#' @return System user name
#' @examples 
#' sysUserName()
#' @export
sysUserName <- function() return(unname(Sys.info()["user"]))

#' Construct message body to insert into GeMS
insertGmtListToGeMSBody <- function(gmtList,
                                    geneFormat=0,
                                    source="PubMed",
                                    xref="",
                                    taxID=9606,
                                    user=sysUserName()) {
  parsed <- lapply(gmtList, function(x) unname(unlist(x)))
  gemsPars <- list(gf=geneFormat,
                   so=source,
                   xref=xref,
                   ti=taxID,
                   us=user)
  dataList <- list(headers=c("setName", "desc", "genes"),
                   parsed=parsed,
                   params=gemsPars)
  return(dataList)
}
#' Insert a GmtList object to GeMS
insertGmtListToGeMS <- function(gmtList,
                                geneFormat=0,
                                source="PubMed",
                                xref="",
                                taxID=9606,
                                user=sysUserName()) {
  dataList <- insertGmtListToGeMSBody(gmtList=gmtList,
                                  geneFormat=geneFormat,
                                  source=source,
                                  xref=xref,
                                  taxID=taxID,
                                  user=user)
  response <- httr::POST(GeMS_INSERT_URL, body=dataList, encode='json')
  returnJSON <- jsonlite::fromJSON(httr::content(response, 'text'))
  return(returnJSON)
}

#' Message body to remove one or gene sets of the same source and user from GeMS
removeFromGeMSBody <- function(setName="", source="", user=sysUserName(), subtype="") {
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
removeFromGeMS <- function(setName="", source="", user=sysUserName(), subtype="") {
  body <- removeFromGeMSBody(setName, source, user, subtype)
  response <- httr::POST(GeMS_REMOVE_URL, body=body, encode='json')
  returnJSON <- jsonlite::fromJSON(httr::content(response, 'text'))
  return(returnJSON)
}

#' Get gene sets of a user from GeMS
#' @param user User name
#' @return A data.frame including following columns: \code{source}, \code{setName}.
#' 
#' @examples 
#' ## my gene-sets
#' getUserSetsFromGeMS()
#' ## from another user
#' getUserSetsFromGeMS("kanga6")
getUserSetsFromGeMS <- function(user=sysUserName()) {
  body <- list(user=user,
               returnParams=list("source", "setName"))
  response <- httr::POST(GeMS_GENESETS_URL , body=body, encode="json")
  returnJSON <-jsonlite::fromJSON(httr::content(response, "text"))
  return(returnJSON$response)
}

