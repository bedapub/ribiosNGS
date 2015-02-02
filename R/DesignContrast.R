design2group <- function(designMatrix) {
  clevels <- apply(designMatrix, 2, ribiosUtils::ulen)
  useCol <- clevels < nrow(designMatrix)
  groups <- apply(designMatrix[,useCol, drop=FALSE],
                  1, paste, collapse="")
  res <- factor(groups)
  levels(res) <- sprintf("AutoGroup_%02d", 1:nlevels(res))
  return(res)
}
DesignContrast <- function(designMatrix, contrastMatrix, groups=NULL) {
  if(is.null(groups))
    groups <- design2group(designMatrix)
  new("DesignContrast",
      design=designMatrix,
      contrasts=contrastMatrix,
      groups=groups)
}

groups <- function(designContrast) return(designContrast@groups)
designMatrix <- function(designContrast) return(designContrast@design)
contrastMatrix <- function(designContrast) return(designContrast@contrasts)
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
parseDesignContrastStr <- function(groupsStr, levelStr, contrastStr) {
  groups <- parseFactor(groupsStr, rlevels=levelStr, make.names=FALSE)
  levels <- levels(groups)
  notvalid <- (levels != make.names(levels))
  if (any(notvalid)) 
    stop("The levels must by syntactically valid names in R, see help(make.names).  Non-valid names: ", 
         paste(levels[notvalid], collapse = ","))
  contrast.vec <- parseContrastStr(contrastStr)
  design <- model.matrix(~0+groups)
  colnames(design) <- levels
  contrasts <- makeContrasts(contrasts=contrasts.vec, levels=levels)
  colnames(contrasts) <- names(contrast.vec)
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrasts,
                        groups=groups)
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
                                sampleGroups=NULL, groupLevels=NULL, contrasts=NULL) {
  if(!is.null(designFile) & !is.null(contrastFile)) {
    return(parseDesignContrastFile(designFile=designFile,
                                   contrastFile=contrastFile,
                                   groupsStr=groupLevels, levelStr=groupLevels))
  } else if (!is.null(sampleGroups) & !is.null(contrasts)) {
    return(parseDesignContrastStr(sampleGroups, groupLevels, contrasts))
  } else {
    stop("Provide either a design matrix and a contrast matrix, or sample groups and contrasts")
  }
}
