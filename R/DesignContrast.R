design2group <- function(designMatrix) {
  clevels <- apply(designMatrix, 2, ribiosUtils::ulen)
  useCol <- clevels < nrow(designMatrix)
  groups <- apply(designMatrix[,useCol, drop=FALSE],
                  1, paste, collapse="")
  res <- factor(groups)
  levels(res) <- sprintf("AutoGroup_%02d", 1:nlevels(res))
  return(res)
}
DesignContrast <- function(designMatrix, contrastMatrix, groups=NULL, dispLevels=NULL) {
  if(is.null(groups))
    groups <- design2group(designMatrix)
  if(is.null(dispLevels))
    dispLevels <- levels(groups)
  new("DesignContrast",
      design=designMatrix,
      contrasts=contrastMatrix,
      groups=groups,
      dispLevels=dispLevels)
}

setMethod("groups", "DesignContrast", function(object) {
  return(object@groups)
})
setMethod("dispGroups", "DesignContrast", function(object) {
  groups <- object@groups
  levels(groups) <- object@dispLevels
  return(groups)
})
setMethod("designMatrix", "DesignContrast", function(object) {
  return(object@design)
})
setMethod("contrastMatrix", "DesignContrast", function(object) {
  return(object@contrasts)
})
setMethod("designVariables", "DesignContrast", function(object) {
  return(colnames(designMatrix(object)))
})
## functions to parse designs and contrasts from files or command-line inputs
parseContrastStr <- function(contrastStr) {
  contrasts <- parseStrings(contrastStr)
  csplit <- strsplit(contrasts, "=")
  hasEqual <- grepl("=", contrasts)
  if(any(hasEqual)) {
    names <- sapply(strsplit(contrasts, "="), "[[", 1L)
    contrasts <- gsub("^.*=", "", contrasts)
    names(contrasts) <- names
  } else {
    names(contrasts) <- contrasts
  }
  return(contrasts)
}
parseDesignContrastStr <- function(groupsStr, levelStr, dispLevelStr, contrastStr) {
  groups <- parseFactor(groupsStr, rlevels=levelStr, make.names=TRUE)
  levels <- levels(groups)
  contrast.vec <- parseContrastStr(contrastStr)
  design <- model.matrix(~0+groups)
  colnames(design) <- levels
  contrasts <- makeContrasts(contrasts=contrast.vec, levels=levels)
  colnames(contrasts) <- names(contrast.vec)
  dispLevels <- parseStrings(dispLevelStr)
  if(is.null(dispLevels)) {
    dispLevels <- levels(groups)
  }
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrasts,
                        groups=groups,
                        dispLevels=dispLevels)
  return(res)
}

parseDesignContrastFile <- function(designFile, contrastFile, groupsStr=NULL, levelStr=NULL) {
  assertFile(designFile)
  assertFile(contrastFile)
  if(!is.null(groupsStr)) {
    groups <- parseFactor(groupsStr, rlevels=levelStr, make.names=FALSE)
    levels <- levels(groups)
    notvalid <- (levels != make.names(levels))
    if (any(notvalid)) 
      stop("The levels must by syntactically valid names in R, see help(make.names).  Non-valid names: ", 
           paste(levels[notvalid], collapse = ","))
  } else {
    groups <- NULL
  }
  design <- readMatrix(designFile)
  contrast <- readMatrix(contrastFile)
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrast,
                        groups=groups)
}
parseDesignContrast <- function(designFile=NULL, contrastFile=NULL,
                                sampleGroups=NULL, groupLevels=NULL, dispLevels=NULL,
                                contrasts=NULL) {
  if(!is.null(designFile) & !is.null(contrastFile)) {
    return(parseDesignContrastFile(designFile=designFile,
                                   contrastFile=contrastFile,
                                   groupsStr=groupLevels, levelStr=groupLevels))
  } else if (!is.null(sampleGroups) & !is.null(contrasts)) {
    return(parseDesignContrastStr(sampleGroups, groupLevels, dispLevels, contrasts))
  } else {
    stop("Provide either a design matrix and a contrast matrix, or sample groups and contrasts")
  }
}
