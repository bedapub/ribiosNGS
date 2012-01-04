## getRcrit and getSubGraphs were copied and adapted from the scorem package
getRcrit <- function (a, n) {
  t <- 0 - qt(a, n - 2)
  sqrt(t^2/(t^2 + n - 2))
}
getSubGraphs <- function (object, alpha, nc, w.crit) {
  r.crit <- getRcrit(alpha, nc)
  cc <- connComp(as(new("graphAM", object > r.crit), "graphNEL"))
  sg <- NULL
  for (x in cc) {
    mx <- object[x, x]
    w <- mean(mx)
    n <- length(x)
    if (n == 2L) {
      if (w < w.crit) {
        x <- as.list(x)
      }
    }
    else if (n == 3L) {
      if (w < w.crit) {
        rs <- c(mx[1L,2L], mx[1L,3L], mx[2L,3L])
        if (max(rs) > r.crit) {
          pairs <- list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
          tog <- pairs[[which.max(rs)]]
          sep <- setdiff(1:3, tog)
          x <- list(x[tog], x[sep])
        }
        else {
          x <- as.list(x)
        }
      }
    }
    else {
      if (w < w.crit) {
        x <- getSubGraphs(mx, alpha, nc - 2, w.crit)
      }
    }
    if (!is.list(x)) {
      x <- list(x)
    }
    sg <- c(sg, x)
  }
  sg
}

kendallWmat <- function(mat,
                        row.factor,
                        summary=c("none", "mean", "median", "max.mean.sig", "max.var.sig"),
                        na.rm=TRUE,
                        alpha=0.01) {
  stopifnot(length(row.factor)==nrow(mat))
  if(!is.factor(row.factor))
    row.factor <- factor(row.factor)
  if(is.null(rownames(mat)) || any(duplicated(rownames(mat))))
    stop("'mat' must have unique rownames\n")
  if(missing(summary))
    summary <- "none"
  summary <- match.arg(summary)
  
  rl <- levels(row.factor)
  rt <- table(row.factor)
  
  nc <- ncol(mat)
  r.crit <- getRcrit(alpha, nc)
  w.crit <- (1L + r.crit)/2L
  
  is.mulFac <- row.factor %in% rl[rt > 1L]
  is.sglFac <- row.factor %in% rl[rt==1L]
  is.naFac <- !(is.mulFac | is.sglFac)
  sglmat <- subset(mat, is.sglFac)
  mulmat <- subset(mat, is.mulFac)
  namat <- subset(mat, is.naFac)
  mulmatFac <- factor(subset(row.factor, is.mulFac))
  
  mul2sgl <- by(mulmat, mulmatFac, function(x) {
    mcor <- cor(t(x), method="spearman")
    w <- mean(mcor, na.rm=TRUE)
    if(w >= w.crit) {
      ids <- list(rownames(x))
    } else {
      ids <- getSubGraphs(mcor, alpha, nc, w.crit)
    }
    if(summary=="none") {
      mm <- x
    } else if (summary=="mean") {
      mm <- t(sapply(ids, function(id) colMeans(x[id,,drop=FALSE], na.rm=TRUE)))
      rownames(mm) <- sapply(ids, "[[", 1)
    } else if (summary=="median") {
      mm <- t(sapply(ids, function(id) apply(x[id,,drop=FALSE],2, median, na.rm=TRUE)))
      rownames(mm) <- sapply(ids, "[[", 1)
    } else if (summary=="max.mean.sig") {
      mmp <- sapply(ids, function(id) id[which.max(rowMeans(x[id, ,drop=FALSE], na.rm=TRUE))])
      mm <- x[mmp,]
      rownames(mm) <- mmp
    } else if (summary=="max.var.sig") {
      mmp <- sapply(ids, function(id) {
        if(length(id)==1)
          return(id)
        id[which.max(apply(x[id,,drop=FALSE], 1, sd, na.rm=TRUE))]
      })
      mm <- x[mmp,]
      rownames(mm) <- mmp
    } else {
      stop("Not implemented summary method\n")
    }
    mm <- data.matrix(mm)
    return(list(matrix=mm, ids=ids))
  })
  
  sgls.names <- paste(row.factor[is.sglFac],
                      rownames(sglmat), sep="|")
  if(summary!="none") {
    muls <- unlist(lapply(mul2sgl, function(x) x$ids), use.names=F, recursive=F)
    muls.names <- paste(rep(levels(mulmatFac), sapply(mul2sgl, function(x) length(x$ids))),
                        sapply(muls, paste, collapse="|"),sep="|")
  } else {
    fp <- rep(levels(mulmatFac), sapply(mul2sgl, function(x) length(unlist(x$ids))))
    fs <- lapply(mul2sgl, function(x) lapply(x$ids,
                                             function(id) rep(paste(id, collapse="|"), length(id))))
    fsl <- unlist(fs, use.names=FALSE, recursive=TRUE)
    muls.names <- paste(fp, fsl, sep="|")
  }
  mulmats <- do.call(rbind, lapply(mul2sgl, function(x) x$matrix))
  stopifnot(length(muls.names) == nrow(mulmats))
  
  res.mat <- rbind(sglmat, mulmats)
  nrf <- factor(c(sgls.names, muls.names))
  nrf.o <- sapply(as.character(nrf),
                  function(x) strsplit(x, "\\|")[[1]][[1]])
  orf <- factor(nrf.o,
                levels=levels(row.factor))
  if(!na.rm) {
    res.mat <- rbind(res.mat, namat)
    ana <- rep(NA, sum(is.naFac))
    orf <- factor(c(as.character(orf),
                    as.character(ana)), levels=levels(orf))
    nrf <- factor(c(as.character(nrf),
                    as.character(ana)), levels=levels(nrf))
    res.df <- data.frame(id=rownames(res.mat),
                         row.factor=orf,
                         new.row.factor=nrf)
  } else {
    res.df <- data.frame(id=rownames(res.mat),
                         row.factor=orf,
                         new.row.factor=nrf)
    
  }

  attr(res.mat, "info") <- res.df
  return(res.mat)
}


setGeneric("kendallW",
           function(object,...) standardGeneric("kendallW"))
setGeneric("kendallWinfo",
           function(object) standardGeneric("kendallWinfo"))
setGeneric("kendallWinfo<-",
           function(object, value) standardGeneric("kendallWinfo<-"))
setMethod("kendallWinfo", "matrix", function(object) {attr(object, "info")})
setReplaceMethod("kendallWinfo", c("matrix","ANY"), function(object, value) {attr(object, "info") <- value; return(object)})
setMethod("kendallW", "matrix", function(object,
                                         row.factor,
                                         summary=c("none", "mean", "median",
                                           "max.mean.sig", "max.var.sig"),
                                         na.rm=TRUE, alpha=0.01) {
  kendallWmat(mat=object, row.factor=row.factor,
              summary=summary, na.rm=na.rm, alpha=alpha)
})
setMethod("kendallW", "ExpressionSet", function(object,
                                                row.factor,
                                                summary=c("none", "mean", "median",
                                                  "max.mean.sig", "max.var.sig"),
                                                na.rm=TRUE, alpha=0.01) {
  exp <- exprs(object)
  new.exp <- kendallWmat(mat=exp, row.factor=row.factor,
                         summary=summary, na.rm=na.rm, alpha=alpha)
  new.info <- kendallWinfo(new.exp)
  kendallWinfo(new.exp) <- NULL
  new.fData <- cbind(fData(object)[match(rownames(new.exp),
                                         featureNames(object)),,drop=FALSE],
                     new.info)
  exprs(object) <- new.exp
  fData(object) <- new.fData
  return(object)
})

