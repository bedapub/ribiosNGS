#' An adapted and enhanced version of limma::camera
#' 
#' @param y 	a numeric matrix of log-expression values or log-ratios of expression values, or any data object containing such a matrix. Rows correspond to probes and columns to samples. Any type of object that can be processed by getEAWP is acceptable.
#' @param index an index vector or a list of index vectors. Can be any vector such that y[index,] of statistic[index] selects the rows corresponding to the test set. The list can be made using \code{ids2indices}.
#' @param design Design matrix
#' @param contrast contrast of the linear model coefficients for which the test is required. Can be an integer specifying a column of design, or else a numeric vector of same length as the number of columns of design.
#' @param weights numeric matrix of observation weights of same size as \code{y}, or a numeric vector of array weights with length equal to \code{ncol(y)}, or a numeric vector of gene weights with length equal to \code{nrow(y)}.
#' @param geneLabels Labels of the features in the input matrix
#' @param use.ranks do a rank-based test (TRUE) or a parametric test (FALSE)?
#' @param allow.neg.cor should reduced variance inflation factors be allowed for negative correlations?
#' ## @param inter.gene.cor numeric, optional preset value for the inter-gene correlation within tested sets. If NA or NULL, then an inter-gene correlation will be estimated for each tested set.
#' @param trend.var logical, should an empirical Bayes trend be estimated? See \code{eBayes} for details.
#' @param sort logical, should the results be sorted by p-value?
#' 
#' The function was adapted from \code{\link[limma]{camera}}, with following improvments
#' \enumerate{
#'   \item The output data.frame is more user-friendly
#'   \item The column 'FDR' is always present, even when only one gene-set was tested
#'   \item Scores are calculated, defined as \code{log10(pValue)*I(directionality)}, where \code{I(directionality)} equals \code{1} if the directionality is \code{Up} and \code{-1} if the directionality is \code{Down}
#'   \item Contributing genes and statistics are printed
#' }
#' 
#' @return 
#' A \code{data.frame} with one row per set and the following columns:
#' \describe{
#'   \item{GeneSet}{Gene set name}
#'   \item{NGenes}{Number of genes in the set}
#'   \item{Correlation}{Estimated correlation}
#'   \item{Direction}{Direction of set-wise regulation, \code{Up} or \code{Down}}
#'   \item{Score}{Gene-set enrichment score, defined as \code{log10(pValue)*I(directionality)}, where \code{I(directionality)} equals \code{1} if the directionality is \code{Up} and \code{-1} if the directionality is \code{Down}}
#'   \item{ContribuingGenes}{A character string, containing all genes labels of genes that are in the set and regulated in the same direction as the set-wise direction, and the respective statistic}
#' }
#' 
#' @note 
#' Since limma 3.29.6, the default setting of allow.neg.cor changes from TRUE to FALSE, and a new parameter, inter.gene.cor, is added with the default value of 0.01, namely a prior inter-gene correlation is set for all gene sets. Currently, \code{biosCamera} does not have  the parameter \code{inter.gene.cor}, but \code{allow.neg.cor} is set by default to \code{FALSE} to be consistent with the latest camera function.
#' 
#' @examples 
#' y <- matrix(rnorm(1000*6),1000,6)
#' design <- cbind(Intercept=1,Group=c(0,0,0,1,1,1))

#' # First set of 20 genes are genuinely differentially expressed
#' index1 <- 1:20
#' y[index1,4:6] <- y[index1,4:6]+1

#' # Second set of 20 genes are not DE
#' index2 <- 21:40

#' biosCamera(y, index1, design)
#' biosCamera(y, index2, design)

#' # compare with the output of camera: columns 'GeneSet', 'Score', 'ContributingGenes' are missing, and in case \code{inter.gene.cor} is (as default) set to a numeric value, the column 'Correlation' is also missing
#' limma::camera(y, index1, design) 
#' limma::camera(y, index1, design, inter.gene.cor=NA)
biosCamera <- function (y, index, design = NULL, contrast = ncol(design), weights = NULL,
                        geneLabels=NULL,
                        use.ranks = FALSE, allow.neg.cor = FALSE, trend.var = FALSE, 
                        sort = FALSE) 
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
    if (nsets > 1) {
        tab$FDR <- p.adjust(tab$PValue, method = "BH")
    } else {
        tab$FDR <- tab$PValue
    }
    tab$Score <- -log10(tab$PValue) * ifelse(Direction=="Up", 1, -1)
    tab$ContributingGenes <- as.character(conts)
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

## the latest CAMERA function from edgeR (3.20.9) and limma (3.34.9)
# limma.camera.default <- function (y, index, design = NULL, contrast = ncol(design), weights = NULL, 
#                             use.ranks = FALSE, allow.neg.cor = FALSE, inter.gene.cor = 0.01, 
#                             trend.var = FALSE, sort = TRUE) {
#   y <- getEAWP(y)
#   G <- nrow(y$exprs)
#   n <- ncol(y$exprs)
#   ID <- rownames(y$exprs)
#   if (G < 3) 
#     stop("Two few genes in dataset: need at least 3")
#   if (!is.list(index)) 
#     index <- list(set1 = index)
#   nsets <- length(index)
#   if (nsets == 0L) 
#     stop("index is empty")
#   if (is.null(design)) 
#     design <- y$design
#   if (is.null(design)) 
#     stop("design matrix not specified")
#   else {
#     design <- as.matrix(design)
#     if (mode(design) != "numeric") 
#       stop("design must be a numeric matrix")
#   }
#   if (nrow(design) != n) 
#     stop("row dimension of design matrix must match column dimension of data")
#   p <- ncol(design)
#   df.residual <- n - p
#   if (df.residual < 1) 
#     stop("No residual df: cannot compute t-tests")
#   if (is.null(weights)) 
#     weights <- y$weights
#   fixed.cor <- !(is.na(inter.gene.cor) || is.null(inter.gene.cor))
#   if (fixed.cor) {
#     if (use.ranks) 
#       df.camera <- Inf
#     else df.camera <- G - 2
#   }
#   else {
#     df.camera <- min(df.residual, G - 2)
#   }
#   y <- y$exprs
#   if (!is.null(weights)) {
#     if (any(weights <= 0)) 
#       stop("weights must be positive")
#     if (length(weights) == n) {
#       sw <- sqrt(weights)
#       y <- t(t(y) * sw)
#       design <- design * sw
#       weights <- NULL
#     }
#   }
#   if (!is.null(weights)) {
#     if (length(weights) == G) 
#       weights <- matrix(weights, G, n)
#     weights <- as.matrix(weights)
#     if (any(dim(weights) != dim(y))) 
#       stop("weights not conformal with y")
#   }
#   if (is.character(contrast)) {
#     contrast <- which(contrast == colnames(design))
#     if (length(contrast) == 0) 
#       stop("coef ", contrast, " not found")
#   }
#   if (length(contrast) == 1) {
#     j <- c((1:p)[-contrast], contrast)
#     if (contrast < p) 
#       design <- design[, j]
#   }
#   else {
#     QR <- qr(contrast)
#     design <- t(qr.qty(QR, t(design)))
#     if (sign(QR$qr[1, 1] < 0)) 
#       design[, 1] <- -design[, 1]
#     design <- design[, c(2:p, 1)]
#   }
#   if (is.null(weights)) {
#     QR <- qr(design)
#     if (QR$rank < p) 
#       stop("design matrix is not of full rank")
#     effects <- qr.qty(QR, t(y))
#     unscaledt <- effects[p, ]
#     if (QR$qr[p, p] < 0) 
#       unscaledt <- -unscaledt
#   }
#   else {
#     effects <- matrix(0, n, G)
#     colnames(effects) <- ID
#     unscaledt <- rep.int(0, G)
#     names(unscaledt) <- ID
#     sw <- sqrt(weights)
#     yw <- y * sw
#     for (g in 1:G) {
#       xw <- design * sw[g, ]
#       QR <- qr(xw)
#       if (QR$rank < p) 
#         stop("weighted design matrix not of full rank for gene ", 
#              g)
#       effects[, g] <- qr.qty(QR, yw[g, ])
#       unscaledt[g] <- effects[p, g]
#       if (QR$qr[p, p] < 0) 
#         unscaledt[g] <- -unscaledt[g]
#     }
#   }
#   U <- effects[-(1:p), , drop = FALSE]
#   sigma2 <- colMeans(U^2)
#   U <- t(U)/sqrt(pmax(sigma2, 1e-08))
#   if (trend.var) 
#     A <- rowMeans(y)
#   else A <- NULL
#   sv <- squeezeVar(sigma2, df = df.residual, covariate = A)
#   modt <- unscaledt/sqrt(sv$var.post)
#   if (use.ranks) 
#     Stat <- modt
#   else {
#     df.total <- min(df.residual + sv$df.prior, G * df.residual)
#     Stat <- zscoreT(modt, df = df.total, approx = TRUE)
#   }
#   meanStat <- mean(Stat)
#   varStat <- var(Stat)
#   tab <- matrix(0, nsets, 5)
#   rownames(tab) <- names(index)
#   colnames(tab) <- c("NGenes", "Correlation", "Down", "Up", 
#                      "TwoSided")
#   for (i in 1:nsets) {
#     iset <- index[[i]]
#     if (is.character(iset)) 
#       iset <- which(ID %in% iset)
#     StatInSet <- Stat[iset]
#     m <- length(StatInSet)
#     m2 <- G - m
#     if (fixed.cor) {
#       correlation <- inter.gene.cor
#       vif <- 1 + (m - 1) * correlation
#     }
#     else {
#       if (m > 1) {
#         Uset <- U[iset, , drop = FALSE]
#         vif <- m * mean(colMeans(Uset)^2)
#         correlation <- (vif - 1)/(m - 1)
#       }
#       else {
#         vif <- 1
#         correlation <- NA
#       }
#     }
#     tab[i, 1] <- m
#     tab[i, 2] <- correlation
#     if (use.ranks) {
#       if (!allow.neg.cor) 
#         correlation <- max(0, correlation)
#       tab[i, 3:4] <- rankSumTestWithCorrelation(iset, statistics = Stat, 
#                                                 correlation = correlation, df = df.camera)
#     }
#     else {
#       if (!allow.neg.cor) 
#         vif <- max(1, vif)
#       meanStatInSet <- mean(StatInSet)
#       delta <- G/m2 * (meanStatInSet - meanStat)
#       varStatPooled <- ((G - 1) * varStat - delta^2 * m * 
#                           m2/G)/(G - 2)
#       two.sample.t <- delta/sqrt(varStatPooled * (vif/m + 
#                                                     1/m2))
#       tab[i, 3] <- pt(two.sample.t, df = df.camera)
#       tab[i, 4] <- pt(two.sample.t, df = df.camera, lower.tail = FALSE)
#     }
#   }
#   tab[, 5] <- 2 * pmin(tab[, 3], tab[, 4])
#   tab <- data.frame(tab, stringsAsFactors = FALSE)
#   Direction <- rep.int("Up", nsets)
#   Direction[tab$Down < tab$Up] <- "Down"
#   tab$Direction <- Direction
#   tab$PValue <- tab$TwoSided
#   tab$Down <- tab$Up <- tab$TwoSided <- NULL
#   if (fixed.cor) 
#     tab$Correlation <- NULL
#   if (nsets > 1) 
#     tab$FDR <- p.adjust(tab$PValue, method = "BH")
#   if (sort && nsets > 1) {
#     o <- order(tab$PValue)
#     tab <- tab[o, ]
#   }
#   tab
# }