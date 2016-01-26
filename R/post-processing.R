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


## helper functions
shortStr <- function(str, n=8) {
    ifelse(nchar(str)>n,
           paste(substr(str, 1, n), "...", sep=""),
           str)
}

fixWidthShortStr <- function(str, n=8, align=c("left", "right")) {
    align <- match.arg(align)
    if(nchar(str)>n) {
        wDots <- 3L
        nwoDots <- n-wDots
        str <- shortStr(str, nwoDots)
    }
    if(nchar(str)<n) {
        emps <- paste(rep(" ", n-nchar(str)),collapse="")
        if(align=="left") {
            str <- paste(str, emps, sep="")
        } else {
            str <- paste(emps, str, sep="")
        }
    }
    return(str)
}


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

jaccardIndex <- function(x, y) length(intersect(x,y))/length(union(x,y))
jaccardDist <- function(x, y) 1-jaccardIndex(x,y)


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
##    cat(fixWidthShortStr(x@Category, wCate, align="right"))
##    cat("  ")
##    cat(fixWidthShortStr(x@Contrast, wContrast, align="right"))
##    cat("  ")
##    cat(fixWidthShortStr(x@GeneSet, wGeneSet, align="right"))
##    cat("  ")
##    cat(fixWidthShortStr(x@Direction, wDirection, align="right"))
##    cat("  ")
##    cat(fixWidthShortStr(sprintf("%1.4f", x@PValue),wP))
##    cat("  ")
##    cat(fixWidthShortStr(sprintf("%1.4f", x@FDR),wFDR), "\n")
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

parseContributingGenes <- function(str) {
    ss <- strsplit(as.character(str), ",")
    genes <- lapply(ss, function(x) gsub("\\(.*\\)", "", x))
    stats <- lapply(ss, function(x) as.numeric(gsub(".*\\((.*)\\)$", "\\1", x)))
    res <- lapply(seq(along=ss), function(i) data.frame(Gene=genes[[i]], Stat=stats[[i]]))
    if(length(res)==1) {
        res <- res[[1]]
    }
    return(res)
}


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
readCameraTable <- function(file) {
    tbl <- readMatrix(file, as.matrix=FALSE, row.names=FALSE)
    tbl$Score <- with(tbl, cameraScore(PValue, Direction))
    return(tbl)
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
    
    scores <- df$scores
    labels <- df$label <- with(df, paste(Category,":",GeneSet,sep=""))
                               
    geneLists <- with(df, split(as.character(Gene), labels))
    gJacSim <- matrix(1L, nrow=length(geneLists), ncol=length(geneLists))
    
    for(i in 1:(length(geneLists)-1))
        for(j in (i+1):length(geneLists))
            gJacSim[i,j] <- gJacSim[j,i] <- jaccardIndex(geneLists[[i]], geneLists[[j]])
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
    
    graphVscores <- matchColumn(V(graph)$name, df, "label")$score
    
    
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
                                score=graphVscores)
    return(list(graph=graph, resTbl=resTbl))
}

visualizeCameraNetworksByContrast <- function(cameraTable, ...) {
    df <- expandSigCameraResults(cameraTable)
    contrasts <- sort(unique(df$Contrast))
    nres <- lapply(as.character(contrasts), function(x) {
                       subdf <- subset(df, Contrast==x)
                       cameraTable2network(subdf, plot=TRUE, ...)
                  })
    tbls <- lapply(nres, function(x) x$resTbl)
    res <- cbind(Contrast=rep(contrasts, sapply(tbls, nrow)),
                 do.call(rbind, tbls))
                      
    return(res)
}
