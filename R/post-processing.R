## for post-processing

## due to speed reasons S4 classes are for now not used
## setClass("GSEresult",
##          representation(Category="character",
##                         Contrast="character",
##                         GeneSet="character",
##                         NGenes="integer",
##                         Correlation="numeric",
##                         Direction="character",
##                         PValue="numeric",
##                         FDR="numeric",
##                         ContributingGenes="data.frame"))
## setClass("GSEresultList", contains="list")


#' Return cameraScore
#' @param pvalue PValues of camera result
#' @param direction directions of camera result, either 'Up' (positive) or 'Down' (negative)
#' @return A numeric vector, containing camera Scores
#' @examples
#' ribiosNGS:::cameraScore(pvalue=c(1, 0.1, 0.01), direction=c("Up", "Down", "Up"))
cameraScore <- function(pvalue, direction) {
    stopifnot(all(direction %in% c("Up", "Down")))
    abs(log10(pvalue)) * ifelse(direction=="Up", 1L, -1L)
}

##print.GSEresult <- function(x, title=TRUE) {
##    wCate <- 15
##    wContrast <- 8
##    wGeneSet <- 25
##    wDirection <- 10
##    wP <- 6
##    wFDR <- 6
##    if(title) {
##        formatStr <- sprintf("%%%ds  %%%ds  %%%ds  %%%ds  %%%ds  %%%ds\n",
##                             wCate, wContrast, wGeneSet, wDirection, wP, wFDR)
##        title <- sprintf(formatStr, "Category","Contrast",
##                         "GeneSet", "Direction",
##                         "PValue","FDR")
##        cat(title)
##    }
##    cat(fixWidthStr(x@Category, wCate, align="right"))
##    cat("  ")
##    cat(fixWidthStr(x@Contrast, wContrast, align="right"))
##    cat("  ")
##    cat(fixWidthStr(x@GeneSet, wGeneSet, align="right"))
##    cat("  ")
##    cat(fixWidthStr(x@Direction, wDirection, align="right"))
##    cat("  ")
##    cat(fixWidthStr(sprintf("%1.4f", x@PValue),wP))
##    cat("  ")
##    cat(fixWidthStr(sprintf("%1.4f", x@FDR),wFDR), "\n")
##}
##
##setMethod("show", "GSEresult",function(object) {
##              print.GSEresult(object, title=TRUE)
##          })
##setMethod("show", "GSEresultList",function(object) {
##              print.GSEresult(object[[1]], title=TRUE)
##              if(length(object)>1) {
##                  for(i in 2:length(object))
##                      print.GSEresult(object[[i]], title=FALSE)
##              }
##          })


#' Parse contributing genes from the output file
#' @param str Character string, containing contributing genes
#' @return A list of \code{data.frames}, each containing two columns, \code{Gene} and \code{Stat}
#' 
#' @examples 
#' parseContributingGenes("AKR1C4(-1.25), AKR1D1(-1.11)")
#' parseContributingGenes(c("AKR1C4(-1.25), AKR1D1(-1.11)",
#'                          "AKT1(1.24), AKT2(1.11), AKT3(1.05)"))
parseContributingGenes <- function(str) {
  ss <- strsplit(str, ",")
  res <- lapply(ss, function(x) {
    genes <- gsub("\\(.*\\)", "", x)
    stat <- as.numeric(as.character(gsub(".*\\((.*)\\)", "\\1", x)))
    return(data.frame(Gene=genes, Stat=stat))
  })
  return(res)
}

#' Parse contributing genes by genesets
#' @param str Character strings, containing contributing genes
#' @param genesets Character strings, geneset labels. Its length must match the length of \code{str}
#' @return A \code{data.frame} containing genesets, genes, and statistics
#' 
#' @examples 
#' parseGenesetsContributingGenes("AKR1C4(-1.25), AKR1D1(-1.11)", "Metabolism")
#' parseGenesetsContributingGenes(c("AKR1C4(-1.25), AKR1D1(-1.11)",
#'                          "AKT1(1.24), AKT2(1.11), AKT3(1.05)"),
#'                          c("Metabolism", "AKTs"))
parseGenesetsContributingGenes <- function(str, genesets) {
  stopifnot(length(str)==length(genesets))
  genes <- parseContributingGenes(str)
  res <- cbind(GeneSet=rep(genesets, sapply(genes, nrow)),
               do.call(rbind, genes))
  return(res)
}

#' #' Parse contributing genesets by both genesets and contrasts
#' #' @param str Character strings, containing contributing genes
#' #' @param genesets Character strings, geneset labels. Its length must match the length of \code{str}
#' #' @return A \code{data.frame} containing genesets, genes, and statistics
#' #' 
#' #' @examples 
#' #' parseGenesetsContributingGenes("AKR1C4(-1.25), AKR1D1(-1.11)", "Metabolism")
#' #' parseGenesetsContributingGenes(c("AKR1C4(-1.25), AKR1D1(-1.11)",
#' #'                          "AKT1(1.24), AKT2(1.11), AKT3(1.05)"),
#' #'                          c("Metabolism", "AKTs"))
#' parseContrastGenesetsContributingGenes <- function(str, genesets, contrasts) {
#'   genes <- parseContributingGenes(str)
#'   res <- cbind(Contrast=rep(contrasts, sapply(contrasts, nrow)),
#'                GeneSet=rep(genesets, sapply(genes, nrow)),
#'                do.call(rbind, genes))
#'   return(res)
#' }



##GSEresult <- function(Category,
##                      Contrast, GeneSet, NGenes, Correlation, Direction, PValue, FDR,
##                      ContributingGenesStr) {
##    contGenes <- parseContributingGenes(as.character(ContributingGenesStr))
##    new("GSEresult",
##        Category=as.character(Category),
##        Contrast=as.character(Contrast),
##        GeneSet=as.character(GeneSet),
##        NGenes=NGenes,
##        Correlation=Correlation,
##        Direction=as.character(Direction),
##        PValue=PValue,
##        FDR=FDR,
##        ContributingGenes=contGenes)
##}
##
##GSEresultListFromDataFrame <- function(df) {
##    grs <- sapply(1:nrow(df),function(i) {
##                      GSEresult(df[i,"Category"],
##                                df[i,"Contrast"],
##                                df[i,"GeneSet"],
##                                df[i,"NGenes"],
##                                df[i,"Correlation"],
##                                df[i,"Direction"],
##                                df[i,"PValue"],
##                                df[i,"FDR"],
##                                df[i,"ContributingGenes"])
##                  })
##    grs <- as(grs, "GSEresultList")
##    return(grs)
##}

##GSEresultListToDataFrame <- function(resList) {
##    category <- sapply(resList, function(x) x@Category)
##    contrast <- sapply(resList, function(x) x@Contrast)
##    geneset <- sapply(resList, function(x) x@GeneSet)
##    direction <- sapply(resList, function(x) x@Direction)
##    pvals <- sapply(resList, function(x) x@PValue)
##    genes <- lapply(resList, function(x) x@ContributingGenes)
##    nGenes <- sapply(genes, nrow)
##    geneTbl <- do.call(rbind, genes)
##    path <- data.frame(Category=rep(category, nGenes),
##                       Contrast=rep(contrast, nGenes),
##                       GeneSet=rep(geneset, nGenes),
##                       Direction=rep(direction, nGenes),
##                       PValue=rep(pvals, nGenes))
##    res <- cbind(path, geneTbl)
##    res$Score <- with(res, cameraScore(PValue, Direction))
##    return(res)
##}

## skip the GSEresultList object, and directly parse camera table
#' Read CAMERA results into a tibble object
#' @param file CAMERA results file
#' @param minNGenes NULL or integer, genesets with fewer genes are filtered out
#' @param maxNGenes NULL or integer, genesets with more genes are filtered out
readCameraResults <- function(file, minNGenes=3, maxNGenes=1000) {
  res <- readr::read_tsv(file, col_types = "cccicddddddddc")
  if(!is.null(minNGenes)) {
    res <- subset(res, NGenes>=minNGenes)
  }
  if(!is.null(maxNGenes)) {
    res <- subset(res, NGenes<=maxNGenes)
  }
  return(res)
}

expandCameraTableGenes <- function(tbl) {
    genes <- tbl$ContributingGenes
    expGenes <- parseContributingGenes(genes)
    isRemove <- colnames(tbl) %in% "ContributingGenes"
    nGenes <- sapply(expGenes, nrow)
    pathTbl <- tbl[rep(1:nrow(tbl),  nGenes), !isRemove]
    geneTbl <- do.call(rbind, expGenes)
    res <- cbind(pathTbl, geneTbl)
    rownames(res) <- NULL
    return(res)
}

expandSigCameraResults <- function(cameraTable,
                                   pVal=0.01, minNGenes=3, includeAllFromSigGenesets=FALSE) {
    if(includeAllFromSigGenesets) {
        sigGeneSets <- subset(cameraTable, PValue<=pVal)$GeneSet
        sigCameraTable <- subset(cameraTable, GeneSet %in% sigGeneSets & NGenes >= minNGenes)
    } else {
        sigCameraTable <- subset(cameraTable, PValue<=pVal & NGenes >= minNGenes)
    }
    expSigCameraTable <- expandCameraTableGenes(sigCameraTable)
    return(expSigCameraTable)
}


cameraTable2network <- function(df, jacThr=0.25, plot=TRUE, ...) {
    retObj <- list(graph=make_empty_graph(),
                   resTbl=data.frame(Category=character(0),
                        GeneSet=character(0),
                       score=numeric(0)))
    if(nrow(df)==0) {
        return(retObj)
    }
    
    scores <- df$Score
    labels <- df$label <- with(df, paste(Category,":",GeneSet,sep=""))
                               
    geneLists <- with(df, split(as.character(Gene), labels))
    gJacSim <- pairwiseJaccardIndex(geneLists)
    gJacBin <- gJacSim;  gJacBin[gJacBin>=jacThr] <- 1L; gJacBin[gJacBin<jacThr] <- 0L
    gLabels <- names(geneLists)
    rownames(gJacBin) <- colnames(gJacBin) <- gLabels


    gCategory <- sapply(strsplit(rownames(gJacBin), ":"), "[[", 1L)
    gGeneSet <- sapply(strsplit(rownames(gJacBin), ":"), function(x) paste(x[2:length(x)], collapse=" "))
    ugCategory <- unique(gCategory)
    for(ugc in ugCategory) {
        if(grepl("^MPS",ugc)) {  ## MPS specific!
            isUgc <- gCategory==ugc
            gJacBin[isUgc, isUgc] <- 1L
        }
    }
        
    graph <- graph_from_adjacency_matrix(gJacBin, mode="undirected", diag=FALSE)
    
    graphVscores <- matchColumn(V(graph)$name, df, "label")$Score
    
    absMaxScore <- max(abs(graphVscores))
    score2range <- as.integer(ribiosPlot:::boundNorm(graphVscores)*100,
                              low=-absMaxScore, high=absMaxScore)
    scoreColor <- royalbluegrayred(101)[score2range+1]

    gOrder <- order(graphVscores)
    layout <- layout_in_circle(graph, order=gOrder)
    if(plot) {
        plot(graph,
             layout=layout,
             vertex.color=scoreColor,
             vertex.label=gsub(":", "\n", gsub("^MPS ", "", gLabels)),  ## MPS specific!
             vertex.label.cex=0.8,
             edge.arrow.size=2, ...)
    }
    retObj$graph <- graph
    retObj$resTbl <- data.frame(Category=gCategory,
                                GeneSet=gGeneSet,
                                Score=graphVscores)
    return(retObj)
}

visualizeCameraNetworksByContrast <- function(expCameraTable, ...) {
    contrasts <- sort(unique(expCameraTable$Contrast))
    nres <- lapply(as.character(contrasts), function(x) {
                       subexpCameraTable <- subset(expCameraTable, Contrast==x)
                       cameraTable2network(subexpCameraTable, plot=TRUE, main=x, ...)
                  })
    tbls <- lapply(nres, function(x) x$resTbl)
    res <- cbind(Contrast=rep(contrasts, sapply(tbls, nrow)),
                 do.call(rbind, tbls))
                      
    return(res)
}
