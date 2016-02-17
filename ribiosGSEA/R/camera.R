## biosCamera: an adaption of limma::camera, with following improvments
## (1) more user-friendly output
## (2) scores calculated based on p-value and on directionality
## (3) contributing genes
biosCamera <- function (y, index, design = NULL, contrast = ncol(design), weights = NULL,
                        geneLabels=NULL,
                        use.ranks = FALSE, allow.neg.cor = TRUE, trend.var = FALSE, 
                        sort = TRUE) 
{
    y <- as.matrix(y)
    G <- nrow(y)
    n <- ncol(y)
    if(is.null(geneLabels)) {
        geneLabels <- rownames(y)
        if(is.null(geneLabels))
            geneLabels <- 1:nrow(y)
    } else {
        haltifnot(length(geneLabels)==nrow(y),
                  msg="geneLabels's length must equal to nrow(y)")
    }
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
    rownames(tab) <- NULL
    colnames(tab) <- c("NGenes", "Correlation", "Down", "Up", 
                       "TwoSided")

    conts <- vector("character", nsets)
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
        if(!is.null(isDown) && !is.na(isDown)) {
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
                                      geneLabels[contInds], contVals), collapse=",")
        }
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

    tab$Score <- -log10(tab$PValue) * ifelse(Direction=="Up", 1, -1)
    tab$ContributingGenes <- conts
    tab$GeneSet <- names(index)
    tab <- putColsFirst(tab, "GeneSet")
    if (sort && nsets > 1) {
        o <- order(tab$PValue)
        tab <- tab[o, ]
    }
    return(tab)
}

## gscCamera: Camera applied to gene set collection
gscCamera <- function(matrix, geneSymbols, gsc, design, contrasts) {
    genes <- gsGenes(gsc)
    genes.inds <- lapply(genes, function(x) {
                             ind <- match(x, geneSymbols)
                             return(ind[!is.na(ind)])
                         })
    names(genes.inds) <- gsNames(gsc)
    cameraRes <- mclapply(1:ncol(contrasts),
                        function(x) {
                            tbl <- biosCamera(matrix,
                                              design=design,
                                              index=genes.inds,
                                              contrast=contrasts[,x],
                                              geneLabels=geneSymbols,
                                              sort=FALSE)
                            if(!"FDR" %in% colnames(tbl)) {
                                ## TRUE if there is only one gene set
                                tbl$FDR <- tbl$PValue
                            }
                            tbl <- tbl[,c("GeneSet", "NGenes", "Correlation", "Direction",
                                          "PValue", "FDR", "ContributingGenes")]
                            tbl <- sortByCol(tbl, "PValue",decreasing=FALSE)
                            return(tbl)
                        })

    cRes <- do.call(rbind, cameraRes)

    if(is.null(colnames(contrasts)))
        colnames(contrasts) <- sprintf("Contrast%d", 1:ncol(contrasts))
    
    bg <- data.frame(Contrast=rep(colnames(contrasts), sapply(cameraRes, nrow)))
    res <- cbind(bg, cRes)
    rownames(res) <- NULL
    res <- subset(res, NGenes>=1 & !is.na(PValue) & !is.nan(PValue))
    return(res)
}
