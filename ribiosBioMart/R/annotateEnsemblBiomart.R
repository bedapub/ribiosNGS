#' Annotate Ensembl Gene IDs with Entrez GeneIDs, gene symbols, and names using Biomart
#' @param ids Ensembl Gene IDs
#' @param mart A Mart or LocalMart object
#' @param multi Logical, in case one ensembl Gene ID is mapped to multiple Entrez GeneIDs, should multiple IDs returned. Default: \code{FALSE}
#' @param orthologue Logical, if \code{TRUE}, human orthologue information (EnsembleGeneID, Entrez GeneID, and HGNC GeneSymbol) will be appended to the result \code{data.frame}. If the dataset is \code{hsapiens}, this option is ignored.
#' @param multiOrth Logical, if \code{TRUE}, one gene may be mapped to more than one human orthologues.
#' 
#' @return A \code{data.frame} of annotations.
#' 
#' @examples 
#' \dontrun{
#'   myMart <- ensemblMart("ensembl", species="hsapiens")
#'   annotateEnsemblBiomart(c("ENSG00000142208"), mart=myMart)
#'   annotateEnsemblBiomart(c("ENSG00000142208",
#'     "ENSG00000105221",
#'     "ENSG00000117020"), mart=myMart)
#'   ## orthologue mapping for mouse
#'   mouseMart <- ensemblMart("ensembl", species="mouse")
#'   annotateEnsemblBiomart(c("ENSMUSG00000001729"), mart=mouseMart)
#'   annotateEnsemblBiomart(c("ENSMUSG00000001729"), mart=mouseMart, orthologue=TRUE)
#'   ## orthologue mapping for rat
#'   ratMart <- ensemblMart("ensembl", species="rat")
#'   annotateEnsemblBiomart(c("ENSRNOG00000028629"), mart=ratMart)
#'   annotateEnsemblBiomart(c("ENSRNOG00000028629"), mart=ratMart, orthologue=TRUE)
#' }
#' 
#' @importFrom biomaRt getBM useDataset
#' @importFrom dplyr as_tibble "%>%" mutate arrange
#' @importFrom ribiosUtils matchColumn
#' @importFrom rlang .data
#' @export
annotateEnsemblBiomart <- function(ids, 
                                   mart=ensemblMart("ensembl", species="hsapiens"),
                                   multi=FALSE, 
                                   orthologue=FALSE, 
                                   multiOrth=FALSE) {
  martDs <- mart@dataset
  isHuman <- grepl("^hsapiens", martDs)
  isMouse <- grepl("^mmusculus", martDs)
  isRat <- grepl("^rnorvegicus", martDs)
  if(isHuman) {
    attrib <- c("ensembl_gene_id", "entrezgene", "hgnc_symbol", "description")
  } else if (isMouse) {
    attrib <- c("ensembl_gene_id", "entrezgene", "mgi_symbol", "description")
  } else if (isRat) {
    attrib <- c("ensembl_gene_id", "entrezgene", "rgd_symbol", "description")
  } else {
    warning("Mapping of genes rather than in human, mouse and rat is not tested so far. Please report any issue!")
    attrib <- c("ensembl_gene_id", "entrezgene", "external_gene_name", "description")
  }


  if(class(mart)=="LocalMart") {
    ## getLocalBM has a problem: the column order is not necessarily correct!
    res <- getLocalBM(attributes=attrib, filters="ensembl_gene_id", 
                                     values=ids, mart=mart)
  } else {
    res <- getBM(attributes=attrib, filters="ensembl_gene_id", values=ids, mart=mart)
  }
  
  res$description <- gsub(" \\[Source:.*\\]$", "", res$description)
  
  resCols <- c("EnsemblGeneID", "GeneID", "GeneSymbol", "GeneName")
  
  colnames(res) <- resCols
  res <- dplyr::as_tibble(res) %>% dplyr::arrange(.data$EnsemblGeneID, .data$GeneID) %>%
    dplyr::mutate(GeneSymbol=replace(.data$GeneSymbol, .data$GeneSymbol=="", NA)) %>%
    dplyr::mutate(GeneName=replace(.data$GeneName, .data$GeneName=="", NA))
  matchedRes <- ribiosUtils::matchColumn(ids, res, "EnsemblGeneID", multi=multi)

  
  ## handle orthologue separately, because attributes from multiple biomaRt pages are not allowed to be used at once
  orthologue <- orthologue && !isHuman
  if(orthologue) {
    orthAttr <- c("ensembl_gene_id", "hsapiens_homolog_ensembl_gene")
    orthRes <- getBM(attributes=orthAttr, filters="ensembl_gene_id", values=ids, mart=mart)
    colnames(orthRes) <- c("EnsemblGeneID", "HumanEnsemblGeneID")
    matchedResMerged <- merge(matchedRes, orthRes, by="EnsemblGeneID", all.x=TRUE, all.y=multiOrth)
    matchedRes  <- ribiosUtils::matchColumn(ids, matchedResMerged, "EnsemblGeneID", multi=multi || multiOrth)
    if(class(mart)=="LocalMart") {
      stop("not implemented")
    } else {
      humanMart <- useDataset("hsapiens_gene_ensembl", mart)
      humanRes <- getBM(attributes=c("ensembl_gene_id", "entrezgene", "hgnc_symbol"),
                       filters="ensembl_gene_id", values=matchedRes$HumanEnsemblGeneID, mart=humanMart)
      humanResMatched <- ribiosUtils::matchColumn(matchedRes$HumanEnsemblGeneID, humanRes, "ensembl_gene_id")
      matchedRes$HumanGeneID <- humanResMatched$entrezgene
      matchedRes$HumanGeneSymbol <- humanResMatched$hgnc_symbol
    }
  }

  rownames(matchedRes) <- NULL
  if(!multi && !multiOrth)
    stopifnot(nrow(matchedRes) == length(ids) & identical(matchedRes$EnsemblGeneID, ids))
  return(matchedRes)
}
