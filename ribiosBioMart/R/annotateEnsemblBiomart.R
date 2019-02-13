#' Annotate Ensembl Gene IDs with Entrez GeneIDs, gene symbols, and names using Biomart
#' @param ids Ensembl Gene IDs
#' @param mart A Mart or LocalMart object
#' @param multi Logical, in case one ensembl Gene ID is mapped to multiple Entrez GeneIDs, should multiple IDs returned. Default: \code{FALSE}
#' @param orthologue Logical, currently ignored
#' @param multiOrth Logical, currently ignored
#' 
#' @examples 
#' \dontrun{
#'   myMart <- ensemblMart("ensembl", species="hsapiens")
#'   annotateEnsemblBiomart(c("ENSG00000142208"), mart=myMart)
#'   annotateEnsemblBiomart(c("ENSG00000142208",
#'     "ENSG00000105221",
#'     "ENSG00000117020"), mart=myMart)
#' }
#' 
#' @importFrom biomaRt getBM
#' @importFrom dplyr as_tibble "%>%" mutate arrange
#' @importFrom ribiosUtils matchColumn
#' @importFrom rlang .data
#' @export
annotateEnsemblBiomart <- function(ids, 
                                   mart=ensemblMart("ensembl", species="hsapiens"),
                                   multi=FALSE, orthologue=FALSE, multiOrth=FALSE) {
  attrib <- c("ensembl_gene_id", "entrezgene", "hgnc_symbol", "description")
  if(class(mart)=="LocalMart") {
    ## getLocalBM has a problem: the column order is not necessarily correct!
    res <- getLocalBM(attributes=attrib, filters="ensembl_gene_id", 
                                     values=ids, mart=mart)
  } else {
    res <- getBM(attributes=attrib, filters="ensembl_gene_id", values=ids, mart=mart)
  }
  
  res$description <- gsub(" \\[Source:.*\\]$", "", res$description)
  colnames(res) <- c("EnsemblGeneID", "GeneID", "GeneSymbol", "GeneName")
  res <- dplyr::as_tibble(res) %>% dplyr::arrange(.data$EnsemblGeneID, .data$GeneID) %>%
    dplyr::mutate(GeneSymbol=replace(.data$GeneSymbol, .data$GeneSymbol=="", NA)) %>%
    dplyr::mutate(GeneName=replace(.data$GeneName, .data$GeneName=="", NA))
  matchedRes <- ribiosUtils::matchColumn(ids, res, "EnsemblGeneID", multi=multi)
  rownames(matchedRes) <- NULL
  if(!multi && !multiOrth)
    stopifnot(nrow(matchedRes) == length(ids))
  return(matchedRes)
}
