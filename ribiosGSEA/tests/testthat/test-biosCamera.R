library(limma)
library(ribiosGSEA)

## benchmark target: limma::camera in version 3.34.9 (below it is a carbon copy)
camera.limma3.34.9 <- function (y, index, design = NULL, contrast = ncol(design), weights = NULL,
            use.ranks = FALSE, allow.neg.cor = FALSE, inter.gene.cor = 0.01,
            trend.var = FALSE, sort = TRUE) {
    y <- getEAWP(y)
    G <- nrow(y$exprs)
    n <- ncol(y$exprs)
    ID <- rownames(y$exprs)
    if (G < 3)
      stop("Two few genes in dataset: need at least 3")
    if (!is.list(index))
      index <- list(set1 = index)
    nsets <- length(index)
    if (nsets == 0L)
      stop("index is empty")
    if (is.null(design))
      design <- y$design
    if (is.null(design))
      stop("design matrix not specified")
    else {
      design <- as.matrix(design)
      if (mode(design) != "numeric")
        stop("design must be a numeric matrix")
    }
    if (nrow(design) != n)
      stop("row dimension of design matrix must match column dimension of data")
    p <- ncol(design)
    df.residual <- n - p
    if (df.residual < 1)
      stop("No residual df: cannot compute t-tests")
    if (is.null(weights))
      weights <- y$weights
    fixed.cor <- !(is.na(inter.gene.cor) || is.null(inter.gene.cor))
    if (fixed.cor) {
      if (use.ranks)
        df.camera <- Inf
      else df.camera <- G - 2
    }
    else {
      df.camera <- min(df.residual, G - 2)
    }
    y <- y$exprs
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
      j <- c((1:p)[-contrast], contrast)
      if (contrast < p)
        design <- design[, j]
    }
    else {
      QR <- qr(contrast)
      design <- t(qr.qty(QR, t(design)))
      if (sign(QR$qr[1, 1] < 0))
        design[, 1] <- -design[, 1]
      design <- design[, c(2:p, 1)]
    }
    if (is.null(weights)) {
      QR <- qr(design)
      if (QR$rank < p)
        stop("design matrix is not of full rank")
      effects <- qr.qty(QR, t(y))
      unscaledt <- effects[p, ]
      if (QR$qr[p, p] < 0)
        unscaledt <- -unscaledt
    }
    else {
      effects <- matrix(0, n, G)
      colnames(effects) <- ID
      unscaledt <- rep.int(0, G)
      names(unscaledt) <- ID
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
    U <- effects[-(1:p), , drop = FALSE]
    sigma2 <- colMeans(U^2)
    U <- t(U)/sqrt(pmax(sigma2, 1e-08))
    if (trend.var)
      A <- rowMeans(y)
    else A <- NULL
    sv <- squeezeVar(sigma2, df = df.residual, covariate = A)
    modt <- unscaledt/sqrt(sv$var.post)
    if (use.ranks)
      Stat <- modt
    else {
      df.total <- min(df.residual + sv$df.prior, G * df.residual)
      Stat <- zscoreT(modt, df = df.total, approx = TRUE)
    }
    meanStat <- mean(Stat)
    varStat <- var(Stat)
    tab <- matrix(0, nsets, 5)
    rownames(tab) <- names(index)
    colnames(tab) <- c("NGenes", "Correlation", "Down", "Up",
                       "TwoSided")
    for (i in 1:nsets) {
      iset <- index[[i]]
      if (is.character(iset))
        iset <- which(ID %in% iset)
      StatInSet <- Stat[iset]
      m <- length(StatInSet)
      m2 <- G - m
      if (fixed.cor) {
        correlation <- inter.gene.cor
        vif <- 1 + (m - 1) * correlation
      }
      else {
        if (m > 1) {
          Uset <- U[iset, , drop = FALSE]
          vif <- m * mean(colMeans(Uset)^2)
          correlation <- (vif - 1)/(m - 1)
        }
        else {
          vif <- 1
          correlation <- NA
        }
      }
      tab[i, 1] <- m
      tab[i, 2] <- correlation
      if (use.ranks) {
        if (!allow.neg.cor)
          correlation <- max(0, correlation)
        tab[i, 3:4] <- rankSumTestWithCorrelation(iset, statistics = Stat,
                                                  correlation = correlation, df = df.camera)
      }
      else {
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
    }
    tab[, 5] <- 2 * pmin(tab[, 3], tab[, 4])
    tab <- data.frame(tab, stringsAsFactors = FALSE)
    Direction <- rep.int("Up", nsets)
    Direction[tab$Down < tab$Up] <- "Down"
    tab$Direction <- Direction
    tab$PValue <- tab$TwoSided
    tab$Down <- tab$Up <- tab$TwoSided <- NULL
    if (fixed.cor)
      tab$Correlation <- NULL
    if (nsets > 1)
      tab$FDR <- p.adjust(tab$PValue, method = "BH")
    if (sort && nsets > 1) {
      o <- order(tab$PValue)
      tab <- tab[o, ]
    }
    tab
  }

## test
set.seed(1887)
y <- matrix(rnorm(1000*6),1000,6)
rownames(y) <- sprintf("Gene%d", 1:nrow(y))
design <- cbind(Intercept=1,Group=c(0,0,0,1,1,1))

## index1: genuinely significantly up-regulated
index1 <- 1:20
y[index1,4:6] <- y[index1,4:6]+1
## index2: not DEs
index2 <- 21:40

gs <- list(GeneSet1=index1, GeneSet2=index2)

(cameraRes <- camera.limma3.34.9(y, gs, design, inter.gene.cor=NA, allow.neg.cor=FALSE))
(cameraModRes <- biosCamera(y, gs, design, .approx.zscoreT = TRUE))
addCols <- c("GeneSet", "Score", "ContributingGenes")
idCols <- c("NGenes", "Correlation", "Direction", "PValue", "FDR")
rownames(cameraRes) <- NULL
test_that("biosCamera performs correctly when inter.gene.cor is estimated", {
  expect_true(all(c(addCols, idCols) %in% colnames(cameraModRes)))
  expect_equal(cameraRes[,idCols], cameraModRes[,idCols])
})


## test no row names
yNoRowNames <- y
rownames(yNoRowNames) <- NULL
(cameraModRes.noRowNames <- biosCamera(yNoRowNames, gs, design))
test_that("biosCamera performs correctly when no row.names are provided", {
  expect_equal(cameraRes[,idCols], cameraModRes.noRowNames[,idCols])
})

## test rankSum test
(cameraRsRes <- camera.limma3.34.9(y, gs, design, use.ranks=TRUE, inter.gene.cor=NA, allow.neg.cor=FALSE))
(cameraRsModRes <- biosCamera(y, gs, design, use.ranks=TRUE))
rownames(cameraRsRes) <- NULL
test_that("biosCamera performs correctly when use.ranks=TRUE", {
  expect_equal(cameraRsRes[,idCols], cameraRsModRes[,idCols])
})

## test fixed correlation
fixIGC <- 0.12
(cameraFixCorRes <- camera.limma3.34.9(y, gs, design, inter.gene.cor=fixIGC))
(biosCameraFixCorRes <- biosCamera(y, gs, design, .fixed.inter.gene.cor = fixIGC, .approx.zscoreT = TRUE))
fixCols <- c("NGenes", "Direction", "PValue", "FDR")
test_that("biosCamera performs correctly when inter-gene correlation is fixed", {
  expect_equivalent(cameraFixCorRes[,fixCols], biosCameraFixCorRes[,fixCols])
})

## test that biosCamera can work with individually defined correlations
biosCameraIndFixCorRes <- biosCamera(y, list(GeneSet1LowCor=gs[[1]],
                                             GeneSet2HighCor=gs[[1]]),
                                     design, 
                                     .fixed.inter.gene.cor=c(0.01, 0.10), 
                                     .approx.zscoreT = TRUE)
cameraLowCorRes <- camera.limma3.34.9(y, gs[[1]],
                            design, inter.gene.cor = 0.01)
cameraHighCorRes <- camera.limma3.34.9(y, gs[[1]],
                                     design, inter.gene.cor = 0.10)
cameraIndFixCorRes <- rbind(cameraLowCorRes, cameraHighCorRes)
indFixCorCols <- c("NGenes", "Direction", "PValue")
test_that("biosCamera performs correctly when inter-gene correlations are specified for each gene set", {
  expect_equivalent(biosCameraIndFixCorRes[,indFixCorCols],
                    cameraIndFixCorRes[, indFixCorCols])
})
