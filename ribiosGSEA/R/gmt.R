matchGenes <- function(x, vec, na.rm=TRUE) {
  if(!is(x, "GeneSets"))
    stop("'x' must be a 'GeneSets' object")
  haltifnot(is.vector(vec),
            msg="'vec' must be a vector of gene names")
  xgl <- gsGenes(x)
  xgenes <- unlist(xgl)
  xgenes[is.na(xgenes) | xgenes=="" | xgenes=="-"] <- NA
  xgenes.ind <- match(xgenes, vec)
  xlens <- sapply(xgl, length)
  xfac <- factor(rep(1:length(xgl), xlens), levels=1:length(xgl))
  gs.indices <- split(xgenes.ind, xfac)
  gs.indices <- lapply(gs.indices, function(x) {
    if(na.rm) x <- x[!is.na(x)]
    return(x)
  })
  names(gs.indices) <- gsName(x)

  return(gs.indices)
}

#' Read GMT file into a GeneSets object
#'
#' @param ... Named or unnamed characater string vector, giving file names of One or more GMT format files. 
#' @param category Character string, category of the gene sets, for instance 'Gene Onotology' or 'Reactome pathway'
#'
#' @examples
#' gmtFile <- system.file("extdata", "example.gmt", package="ribiosGSEA")
#' mySet <- readGmt(gmtFile)
#' myFakeSet <- readGmt(CategoryA=gmtFile, CategoryB=gmtFile)
#' anotherFakeSet <- readGmt(gmtFile, gmtFile, category=c("CategoryA", "CategoryB"))
#' 
readGmt <- function(..., category=NULL) {
    files.list <- list(...)
    files <- unlist(files.list, use.names=TRUE)
    fnames <- names(files)
    if(!is.null(fnames)) {
        if(!is.null(category))
            warning("'file' have names - the category option is ignored")
        category <- fnames
    } else if(is.null(category)) {
        category <- basefilename(files)
    } else if(!is.null(category)) {
        category <- rep(category, length.out=length(files))
    }
    gsList <- lapply(files, read_gmt_list)
    gs <- unlist(gsList, use.names=TRUE, recursive=FALSE)
    category <- rep(category, sapply(gsList, length))
    resl <- lapply(seq(along=gs), function(i) {
                       return(GeneSet(category=category[[i]],
                                      name=gs[[i]]$name,
                                      desc=gs[[i]]$desc,
                                      genes=gs[[i]]$genes))
                   })
    names(resl) <- sapply(resl, gsName)
    res <- GeneSets(resl)
    return(res)
}

#' Write an GeneSet object into a file
#'
#' @param geneSet A GeneSet object
#' @param file Character string, output file name
#'
#' @examples
#' gmtFile <- system.file("extdata", "example.gmt", package="ribiosGSEA")
#' mySet <- readGmt(gmtFile)[1:5]
#' myTempFile <- tempfile()
#' writeGmt(mySet, file=myTempFile)
#' readLines(myTempFile)
writeGmt <- function(geneSet, file) {
    gmtList <- lapply(seq(along=geneSet), function(i) {
                          list(name=gsName(geneSet[[i]]),
                               description=paste(gsCategory(geneSet[[i]]), gsDesc(geneSet[[i]]), sep="|"),
                               genes=gsGenes(geneSet[[i]]))
                      })
    ribiosIO::write_gmt(gmtList, file=file)
}

parseGmt <- function(file, vec, min, max) {
  res <- readGmt(file)
  ind <- matchGenes(res, vec, na.rm=TRUE)

  ## final filtering
  nmin <- ifelse(!missing(min) && is.numeric(min), min, 0)
  nmax <- ifelse(!missing(max) && is.numeric(max), max, Inf)
  ind <- ind[sapply(ind, function(x) length(x)>=nmin & length(x)<=nmax)]
  return(ind)
}

appendOneGeneSets <- function(geneSets, newGeneSets) {
   res <- geneSets
   res@.Data <- c(geneSets@.Data, newGeneSets@.Data)
   names(res) <- c(names(geneSets), names(newGeneSets))
   return(res)
}

#' Append one or more instances of GeneSets to an existing instance
#'
#' @param geneSets An existing instance of GeneSets to which other instances should be appended
#' @param newGeneSets An instance of GeneSets that will be appended to \code{geneSets}
#' @param ... Other instance(s) of GeneSets that will be appended consequentially
#'
#' @examples
#' gmtFile <- system.file("extdata", "example.gmt", package="ribiosGSEA")
#' mySet <- readGmt(gmtFile)
#' myFakeSet <- readGmt(CategoryA=gmtFile, CategoryB=gmtFile)
#' anotherFakeSet <- readGmt(gmtFile, gmtFile, category=c("CategoryC", "CategoryD"))
#' mySetAppended <- appendGeneSets(mySet, myFakeSet)
#' mySetAppendedTwice <- appendGeneSets(mySet, myFakeSet, anotherFakeSet)
appendGeneSets <- function(geneSets, newGeneSets, ...) {
    res <- appendOneGeneSets(geneSets, newGeneSets)
    newList <- list(...)
    for(ngs in newList) {
        res <- appendOneGeneSets(res, ngs)
    }
    return(res)
}

#' Read molecular-phenotyping genesets
#' 
#' @param file GMT file which stores default molecular-phenotyping genesets
#' 
#' @return A \code{GeneSet} object containing molecular-phenotypic screening (MPS) categories and genes
 
readMPSGmt <- function(file) {
  gs <- read_gmt_list(file)
  category <- sprintf("MPS %s", sapply(gs, function(x) x$description))
  resl <- lapply(seq(along = gs), function(i) {
    return(GeneSet(category = category[[i]], name = gs[[i]]$name, 
                   desc = gs[[i]]$description, genes = gs[[i]]$genes))
  })
  names(resl) <- sapply(resl, gsName)
  res <- GeneSets(resl)
  return(res)
}

#' Read default genesets for gene-set enrichment analysis
#'
#' In Roche Bioinformatics we use a default collection of gene-sets for gene-set enrichment analysis. This function loads this collection.
#'
#' @param path Character, path to the directory where the gmt files are stored
#' @param mps Logical,  whether molecular-phenotypic screening (MPS) genesets should be read in as pathway-centric categories (\code{TRUE}) or as one category named \code{MolecularPhenotyping} (\code{FALSE}).
#' 
#' @details
#'
#' The default collection includes both publicly available genesets as well as proprietary genesets, and therefore they are not included as part of the ribios package.
#'
#' Publicly available genesets include
#' \itemize{
#'  \item{MSigDB: collections C2, C7 and Hallmark}
#'  \item{RONET: which is a collection of publicly available pathway databases including REACTOME and NCI-Nature}
#'  \item{goslim}
#' }
#' @examples 
#' \dontrun{
#'   readDefaultGenesets("/tmp/defaultGmts")
#' }
readDefaultGenesets <- function(path,
                                mps=FALSE) {
  assertFile(msigdb.c2.file <- file.path(path, "msigdb.c2.all.symbols.gmt"))
  assertFile(msigdb.c7.file <- file.path(path, "msigdb.c7.all.symbols.gmt"))
  assertFile(msigdb.hallmark.file <- file.path(path, "msigdb.hallmark.all.symbols.gmt"))
  
  assertFile(mps.pathway <- file.path(path, "MolecularPhenotyping-genesets.gmt"))
  
  assertFile(ronet.file <- file.path(path, "path.ronet.roche.symbols.gmt"))
  assertFile(goslim.file <- file.path(path, "go_slim.bp.roche.symbols.gmt"))

  assertFile(upstream.file <- file.path(path, "MetaBase.downstream.expression.gmt"))  
  assertFile(mbdisease.file <- file.path(path, "MetaBase.DiseaseBiomarker.gmt"))
  assertFile(mbmetabolic.file <- file.path(path, "MetaBase.Metabolic.gmt"))
  assertFile(mbpath.file <- file.path(path, "MetaBase.PathwayMap.gmt"))
  assertFile(mbtoxicity.file <- file.path(path, "MetaBase.Toxicity.gmt"))
  assertFile(mbpathology.file <- file.path(path, "MetaBase.ToxicPathology.gmt"))
  assertFile(mbprocess.file <- file.path(path, "MetaBase.TRprocesses.gmt"))

  assertFile(immunomics.file <- file.path(path, "exp.immune.roche.symbols.gmt"))
  assertFile(immunespace.file <- file.path(path, "immunespace.gmt"))
  
  if(mps) {
    mpsGSCs <- readMPSGmt(mps.pathway)
    gscs <- readGmt(msigdbC2=msigdb.c2.file,
                    msigdbC7=msigdb.c7.file,
                    msigdbHallmark = msigdb.hallmark.file,
                    immunomics = immunomics.file,
                    immunespace = immunespace.file)
    gscs <- appendGeneSets(mpsGSCs, gscs)
  } else {
    gscs <- readGmt(MolecularPhenotyping=mps.pathway,
                    upstream=upstream.file,
                    ronet=ronet.file,
                    goslim=goslim.file,
                    mbdisease=mbdisease.file,
                    mbmetabolic=mbmetabolic.file,
                    mbpath=mbpath.file,
                    mbprocess=mbprocess.file,
                    mbtoxicity=mbtoxicity.file,		
                    mbpathology=mbpathology.file,
                    msigdbC2=msigdb.c2.file,
                    msigdbC7=msigdb.c7.file,
                    msigdbHallmark = msigdb.hallmark.file,
                    immunomics = immunomics.file,
                    immunespace = immunespace.file)
  }
  return(gscs)
}
