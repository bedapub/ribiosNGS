## TODO: replace data.frame with Objects

## gage method
myGage <- function(logFC, gsc) {
    genes <- gsGenes(gsc)
    cate <- gsCategory(gsc)
    gdf <- data.frame(geneset=names(genes), category=cate)
    gage.res <- gage::gage(logFC, gsets=genes, ref=NULL, samp=NULL)
    greater <- as.data.frame(gage.res[[1]][,c("stat.mean", "p.val", "q.val", "set.size")])
    less <- as.data.frame(gage.res[[2]][,c("p.val", "q.val")])
    greater$geneset <- rownames(greater)
    less$geneset <- rownames(less)
    greater <- merge(greater, gdf, by="geneset")
    less <- merge(less, gdf, by="geneset")
    res.raw <- merge(greater, less, by=c("geneset","category"),
                     suffix=c(".greater", ".less"))
    direction <- with(res.raw, ifelse(p.val.less<p.val.greater, "Down", "Up"))
    pVal.pmin <- with(res.raw, ifelse(p.val.less<p.val.greater, p.val.less, p.val.greater))
    pVal <- pVal.pmin * 2; pVal[pVal>1] <- 1

    ## TODO: contributing genes

    res <- data.frame(Category=res.raw$category,
                      GeneSet=res.raw$geneset,
                      NGenes=res.raw$set.size,
                      Direction=direction,
                      PValue=pVal,
                      FDR=rep(NA,length(pVal)))
    res <- subset(res, NGenes>=1 & !is.na(PValue) & !is.nan(PValue))
    res$FDR <- p.adjust(res$PValue, "fdr")
    return(res)
}


logFCgage <- function(edgeResult, gscs) {
    geneSymbols <- fData(edgeResult)$GeneSymbol
    logFCs <- lapply(dgeTables(edgeResult), function(x) {
                         res <- x$logFC
                         names(res) <- x$GeneSymbol
                         return(res)
                     })
    erTables <- lapply(logFCs, function(x) myGage(x, gscs))
    
    erTable <- do.call(rbind, erTables)
    erTable$Contrast <- rep(names(logFCs),sapply(erTables, nrow))
    erTable <- putColsFirst(erTable, c("Category", "Contrast", "GeneSet"))
    rownames(erTable) <- NULL
    
    edgeGse <- as(edgeResult, "EdgeGSE")
    edgeGse@geneSets <- gscs
    edgeGse@method <- "gage"
    edgeGse@enrichTables <- erTable
    return(edgeGse)
}

##----------------------------------------##
## camera
##----------------------------------------##

## cameraContr: modified camera method, with contributing genes in output
cameraContr <- function (y,
                        index,
                        design = NULL,
                        contrast = ncol(design),
                        weights = NULL, 
                        use.ranks = FALSE, allow.neg.cor = TRUE, trend.var = FALSE, 
                        sort = TRUE) {
    y <- as.matrix(y)
    G <- nrow(y)
    n <- ncol(y)
    if (!is.list(index)) 
        index <- list(set1 = index)
    if (is.null(design)) 
        stop("no design matrix")
    p <- ncol(design)
    df.residual <- n - p
    df.camera <- min(df.residual, G - 2)
    if (!is.null(weights)) {
        if (any(weights <= 0)) 
            stop("weights must be positive")
        if (length(weights) == n) {
            sw <- sqrt(weights)
            y <- t(t(y) * sw)
            design <- design * sw
            weights <- NULL
        }
    }
    if (!is.null(weights)) {
        if (length(weights) == G) 
            weights <- matrix(weights, G, n)
        weights <- as.matrix(weights)
        if (any(dim(weights) != dim(y))) 
            stop("weights not conformal with y")
    }
    if (is.character(contrast)) {
        contrast <- which(contrast == colnames(design))
        if (length(contrast) == 0) 
            stop("coef ", contrast, " not found")
    }
    if (length(contrast) == 1) {
        if(contrast < p) {
            j <- c((1:p)[-contrast], contrast)
            design <- design[, j] ## JDZ: this if-trunk reorders the to-be-tested contrast to the last column of the design matrix
        }
    }  else {
        QR <- qr(contrast)
        design <- t(qr.qty(QR, t(design)))
        if (sign(QR$qr[1, 1] < 0)) 
            design[, 1] <- -design[, 1]
        design <- design[, c(2:p, 1)] ## JDZ: this else-trunk 'trransforms' the design matrix into a new one with the contrast and reorders the to-be-tested contrast to the last column of the design matrix. I understand that what we estimate is in fact a linear transformation of coefficients of the linear model (beta == C^T %*% alpha, where alpha denotes coefficients and C^T denotes contrasts), it seems that the QR decomposition of the contrast matrix is used to re-parameterize the design matrix so as to encode the desired contrast directly in one of the columns in the design matrix. This is however just a guess and needs verification. 
    }
    if (is.null(weights)) {
        QR <- qr(design)
        if (QR$rank < p) 
            stop("design matrix is not of full rank")
        effects <- qr.qty(QR, t(y))
        unscaledt <- effects[p, ]
        if (QR$qr[p, p] < 0) 
            unscaledt <- -unscaledt
    }  else {
        effects <- matrix(0, n, G)
        unscaledt <- rep(0, n)
        sw <- sqrt(weights)
        yw <- y * sw
        for (g in 1:G) {
            xw <- design * sw[g, ]
            QR <- qr(xw)
            if (QR$rank < p) 
                stop("weighted design matrix not of full rank for gene ", 
                  g)
            effects[, g] <- qr.qty(QR, yw[g, ])
            unscaledt[g] <- effects[p, g]
            if (QR$qr[p, p] < 0) 
                unscaledt[g] <- -unscaledt[g]
        }
    }

    ## JDZ: effects is a n x G matrix (n=ncol(y), G=nrow(y))
    U <- effects[-(1:p), , drop = FALSE] ## JDZ: only takes the residuals 
    sigma2 <- colMeans(U^2)
    U <- t(U)/sqrt(sigma2)
    if (trend.var) 
        A <- rowMeans(y)
    else A <- NULL
    sv <- squeezeVar(sigma2, df = df.residual, covariate = A)
    modt <- unscaledt/sqrt(sv$var.post)
    df.total <- min(df.residual + sv$df.prior, G * df.residual)
    Stat <- zscoreT(modt, df = df.total)
    meanStat <- mean(Stat)
    varStat <- var(Stat)
    nsets <- length(index)
    tab <- matrix(0, nsets, 5)
    rownames(tab) <- names(index)
    colnames(tab) <- c("NGenes", "Correlation", "Down", "Up", 
                       "TwoSided")

    conts <- vector("character", nsets)
    rn <- rownames(y)
    if(is.null(rn)) rn <- as.character(1:G)
    ## JDZ: notice that no matter whether rank is used or not, the statistic underlying the camera method is always the moderated t statistic
    for (i in 1:nsets) {
        iset <- index[[i]]
        StatInSet <- Stat[iset]
        m <- length(StatInSet)
        m2 <- G - m
        if (m > 1) {
            Uset <- U[iset, , drop = FALSE]
            vif <- m * mean(colMeans(Uset)^2)
            correlation <- (vif - 1)/(m - 1)
        }
        else {
            vif <- 1
            correlation <- NA
        }
        tab[i, 1] <- m
        tab[i, 2] <- correlation
        if (use.ranks) {
            if (!allow.neg.cor) 
                correlation <- max(0, correlation)
            tab[i, 3:4] <- rankSumTestWithCorrelation(iset, statistics = Stat, 
                correlation = correlation, df = df.camera)
        } else {
            if (!allow.neg.cor) 
                vif <- max(1, vif)
            meanStatInSet <- mean(StatInSet)
            delta <- G/m2 * (meanStatInSet - meanStat)
            varStatPooled <- ((G - 1) * varStat - delta^2 * m * 
                m2/G)/(G - 2)
            two.sample.t <- delta/sqrt(varStatPooled * (vif/m + 
                1/m2))
            tab[i, 3] <- pt(two.sample.t, df = df.camera)
            tab[i, 4] <- pt(two.sample.t, df = df.camera, lower.tail = FALSE)
        }
        isDown <- tab[i,3] <= tab[i,4]
        if(isDown) { ## pDown < pUp
            contInds <- iset[StatInSet<meanStat]
        } else {
            contInds <- iset[StatInSet>meanStat]
        }
        contVals <- Stat[contInds]
        contOrd <- order(contVals, decreasing=!isDown)
        contInds <- contInds[contOrd]
        contVals <- contVals[contOrd]
        
        conts[i] <- paste(sprintf("%s(%1.2f)",
                                  rn[contInds], contVals), collapse=",")
    }
    
    tab[, 5] <- 2 * pmin(tab[, 3], tab[, 4])
    tab <- data.frame(tab, stringsAsFactors = FALSE)
    Direction <- rep.int("Up", nsets)
    Direction[tab$Down < tab$Up] <- "Down"
    tab$Direction <- Direction
    tab$PValue <- tab$TwoSided
    tab$Down <- tab$Up <- tab$TwoSided <- NULL
    if (nsets > 1) 
        tab$FDR <- p.adjust(tab$PValue, method = "BH")
    tab$ContributingGenes <- conts
    if (sort && nsets > 1) {
        o <- order(tab$PValue)
        tab <- tab[o, ]
    }
    tab
}



voomCameraGsc <- function(voom, geneSymbols, gsc, design, contrasts) {
  genes <- gsGenes(gsc)
  genes.inds <- lapply(genes, function(x) {
    ind <- match(x, geneSymbols)
    return(ind[!is.na(ind)])
  })

  
  cameraRes <- lapply(1:ncol(contrasts),
                      function(x) {
                          tbl <- cameraContr(voom,
                                             design=design,
                                             index=genes.inds,
                                             contrast=contrasts[,x], sort=FALSE)
                          tbl$GeneSet <- names(genes.inds)
                          rownames(tbl) <- NULL
                          if(!"FDR" %in% colnames(tbl)) {
                              ## TRUE there is only one gene set
                              tbl$FDR <- tbl$PValue
                          }
                          tbl <- tbl[,c("GeneSet", "NGenes", "Correlation", "Direction",
                                        "PValue", "FDR", "ContributingGenes")]
                          tbl <- sortByCol(tbl, "PValue")
                          return(tbl)
                      })
  cRes <- do.call(rbind, cameraRes)
  bg <- data.frame(Contrast=rep(colnames(contrasts), sapply(cameraRes, nrow)))
  res <- cbind(bg, cRes)
  rownames(res) <- NULL
  res <- subset(res, NGenes>=1 & !is.na(PValue) & !is.nan(PValue))
  return(res)
}
voomCamera <- function(edgeObj, gscs) {
  obj.voom <- voom(dgeList(edgeObj))
  ctnames<- contrastNames(edgeObj)
  design <- designMatrix(edgeObj)
  ct <- contrastMatrix(edgeObj)
  geneSymbols <- fData(edgeObj)$GeneSymbol

  categories <- gsCategory(gscs)
  erTables <- tapply(gscs, categories, function(gsc) {
                         tt <- voomCameraGsc(obj.voom,
                                             geneSymbols,
                                             gsc=gsc, design=design, contrasts=ct)
                         return(tt)
                     })
  erTable <- do.call(rbind, erTables)
  erTable$Category <- rep(names(erTables), sapply(erTables, nrow))
  erTable <- putColsFirst(erTable, "Category")
  rownames(erTable) <- NULL
  
  edgeGse <-   as(edgeObj,"EdgeGSE")
  edgeGse@geneSets <- gscs
  edgeGse@method <- "voom+camera"
  edgeGse@enrichTables <- erTable
  return(edgeGse)
}

fullEnrichTable <- function(edgeGse) {
  return(edgeGse@enrichTables)
}
