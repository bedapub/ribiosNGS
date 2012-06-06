## read and transform Q-values from GSEA output files
gseaQvalue <- function(file, threshold=1E-4, log=FALSE, posLog=FALSE) {
  tbl <- read.table(file, sep="\t", header=TRUE)
  name <- tbl[,"NAME"]
  q <- tbl[,"FDR.q.val"]
  if(!is.null(threshold) && !is.na(threshold))
    q[q<threshold] <- threshold
  if(log)
    q <- log(q)
  if(posLog)
    q <- (-q)
  return(data.frame(name=name, value=q))
}
## read enrichment scores from GSEA output files
gseaES <- function(file, normalized=FALSE) {
  tbl <- read.table(file, sep="\t", header=TRUE)
  name <- tbl[,1L]
  vc <- ifelse(normalized, "NES", "ES")
  v <- tbl[,vc]
  return(data.frame(name=name, value=v))
}

## read finger print from GSEA directory
gseaFingerprint <- function(gseaDir, value=c("q", "es", "nes"), threshold=1E-4, sortByName=TRUE) {
  value <- match.arg(value)
  xls <- dir(gseaDir, pattern="gsea_report_for_.*\\.xls")
  pos.xls <- grep("gsea_report_for_.*_pos_.*\\.xls", xls)
  neg.xls <- grep("gsea_report_for_.*_neg_.*\\.xls", xls)

  if(length(pos.xls)==1L) {
    if(value=="q") {
      poss <- gseaQvalue(file.path(gseaDir, xls[pos.xls]), threshold=threshold, log=TRUE, posLog=TRUE)
    } else {
      nes <- ifelse(value=="nes", TRUE, FALSE)
      poss <- gseaES(file.path(gseaDir, xls[pos.xls]), normalized=nes)
    }
  } else {
    poss <- NULL
  }
  if(length(neg.xls)==1L) {
    if(value=="q") {
      negs <- gseaQvalue(file.path(gseaDir, xls[neg.xls]), threshold=threshold, log=TRUE, posLog=FALSE)
    } else {
      nes <- ifelse(value=="nes", TRUE, FALSE)
      negs <- gseaES(file.path(gseaDir, xls[neg.xls]), normalized=nes)
    }
  } else {
    negs <- NULL
  }
  paths <- rbind(poss, negs)
  if(!is.null(paths) && sortByName) {
    paths <- sortByCol(paths, "name", decreasing=FALSE)
  }
  return(paths)
}

gseaFingerprintMatrix <- function(gseaDirs, value="es",...) {
  hs.fps <- lapply(gseaDirs,gseaFingerprint, value=value,...)
  fps <- hs.fps[!sapply(hs.fps, is.null)]
  gseaNames <- gsub("\\.GseaPreranked\\.[0-9]*$", "", basename(gseaDirs))
  fps.names <- unique(unlist(lapply(fps, function(x) x[,1L])))
  fpsMat <- matrix(NA, nrow=length(fps.names), ncol=length(fps),
                   dimnames=list(fps.names, gseaNames))
  for(i in seq(along=fps)) {
    fpsMat[match(fps[[i]][,1L], fps.names),i] <- fps[[i]][,2L]
  }
  return(fpsMat)
}
