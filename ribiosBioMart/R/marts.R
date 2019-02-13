#' Get Ensembl species name
#' @param species Character string, either the common name of commonly used species (see below), or the species name in the form of \code{Homo sapiens} or \code{H. sapiens}
#' @return The Ensembl species name in the form of \code{hsapiens}
#' 
#' @note 
#' Commonly used species include: human, mouse, rat, rabbit, chimpanzee, cyno (crab-eating monkey), 
#' dog, pig, chicken, and zebrafish
#' 
#' @examples 
#' ## species names
#' ensemblSpeciesName("Homo sapiens")
#' ensemblSpeciesName("H.sapiens")
#' ensemblSpeciesName("H. sapiens")
#' ensemblSpeciesName("Gallus gallus")
#' ## common names
#' ensemblSpeciesName("human")
#' ensemblSpeciesName("pig")
#' ## in case the input name is already valid Ensembl name
#' ensemblSpeciesName("hsapiens")
#' @export
ensemblSpeciesName <- function(species="hsapiens") {
  species <- tolower(species)
  twopartName <- switch(species,
                        "human"="hsapiens",
                        "mouse"="mmusculus",
                        "rat"="rnorvegicus",
                        "rabbit"="ocuniculus",
                        "chimpanzee"="ptroglodytes",
                        "cyno"="mfascicularis",
                        "dog"="cfamiliaris",
                        "pig"="sscrofa",
                        "chicken"="ggallus",
                        "zebrafish"="drerio",
                        NA)
  if(!is.na(twopartName)) {
    return(twopartName)
  } else { ## possible: homo sapiens, h.sapiens
    if(!grepl("\\s", species) && !grepl("[[:punct:]]", species))
      return(species)
    punctTwoSpace <- gsub("\\s+", " ", gsub("[[:punct:]]", " ",  species))
    strs <- strsplit(punctTwoSpace, " ")[[1]]
    res <- paste(substr(strs[1],1,1),
                 strs[length(strs)], sep="")
    return(res)
  }
}

#' Construct the Ensembl mart dataset name using species name
#' @param species Character string, the species name (e.g. \code{H. sapiens}). Common names are accepted for commonly used species.
#' @return The dataset name of the Ensembl mart of that species
#' 
#' @seealso \code{\link{ensemblSpeciesName}}
#' @examples 
#' ensemblMartDataset("hsapiens")
#' ensemblMartDataset("mmusculus")
#' 
#' @export
ensemblMartDataset <- function(species="hsapiens") {
  species <- ensemblSpeciesName(species)
  if(!grepl("_gene_ensembl$", species)) {
    species <- paste(species, "_gene_ensembl", sep="")
  }
  return(species)
}
#' Construct a Mart object with the dataset of the given species
#' 
#' @param mart Character string, mart to be used, "ensembl" by default.
#' @param species Character string, species name to be used. Common names are accepted for commonly used species.
#' @param ... Other parameters passed to \code{\link[biomaRt]{useMart}}
#' @return A \code{Mart} object of the selected dataset
#' 
#' @seealso \code{\link{ensemblSpeciesName}}, \code{\link{ensemblMartDataset}}
#' 
#' @examples 
#' \dontrun{
#'   humanMart <- ensemblMart(species="human")
#'   ratMart <- ensemblMart(species="rnorvegicus")
#' }
#' @importFrom biomaRt useMart
#' @export
ensemblMart <- function(mart="ensembl", species="hsapiens", ...) {
  amart <- useMart(mart)
  dataset <- ensemblMartDataset(species)
  if(!dataset %in% listDatasets(amart)$dataset) {
    stop(sprintf("species '%s' not supported in the selected mart", species))
  }
  smart <- useMart(mart, dataset=dataset, ...)
  return(smart)
}

#' Construct a LocalMart object to access Roche internal Ensembl Mart
#' @param species Species to be used
#' @param ensembl_version Ensembl version to be used
#' @param host Character string, host server address
#' @param port Integer, port number
#' @param user Character string, user name
#' @param passwd Character string, password
#' 
#' @return A \code{LocalMart} object which can be used to access Roche internal Ensembl Mart
#' 
#' @note 
#' If you are within Roche, the credentials can be obtained at [https://rochewiki.roche.com/confluence/display/BEDA/BIOMART+@+Roche](https://rochewiki.roche.com/confluence/display/BEDA/BIOMART+@+Roche).
#' 
#' @examples 
#' \dontrun{
#'   rocheEnsembleMart(species="hsapiens")
#' }
rocheEnsemblMart <- function(species="hsapiens",
                             ensembl_version=95,  ## currently 95/94 are supported
                             host=NULL,
                             port=NULL,
                             user=NULL, 
                             passwd=NULL) {
  if(is.null(host) || is.null(port) || is.null(user) || is.null(passwd)) {
    stop("'host', 'port', 'user' and 'passwd' must be specified.")
  }
  con <- EnsemblDBCredentials(host = host,
                              port = port,
                              user = user,
                              passwd = passwd,
                              ensembl_version=ensembl_version)
  
  dataset <- ensemblMartDataset(species)
  mart <- ribiosBioMart::useLocalMart(con, dataset=dataset)
  return(mart)
}
