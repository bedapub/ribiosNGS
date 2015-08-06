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
  res <- new("DesignContrast",
             design=designMatrix,
             contrasts=contrastMatrix,
             groups=groups,
             dispLevels=dispLevels)
  return(res)
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

parseDesignContrastFile <- function(designFile, contrastFile,
                                    groupsStr=NULL, levelStr=NULL,
                                    dispLevelStr) {
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
  dispLevels <- parseStrings(dispLevelStr)
  if(is.null(dispLevels)) {
    dispLevels <- levels(groups)
  }
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrast,
                        groups=groups,
                        dispLevels=dispLevels)
  return(res)
}
plainFile2ConcString <- function(str) {
    if(!is.null(str) && file.exists(str)) {
        str <- paste(readLines(str), collapse=",")
    }
    return(str)
}

#' Parse study design and asked questions encoded in design and contrast matrices or in one-way ANOVA designs
#' @param designFile: A plain tab-delimited file with headers encoding the design matrix, or NULL
#' @param contrastFile: A plain tab-delimited file with headers encoding the contrast matrix, or NULL
#' @param sampleGroups: A character string concatenated by commas (e.g. A,B,C), or a plain text file containing one string per line (e.g. A\emph{newline}B\emph{newline}C), encoding sample group memberships.
#' @param groupLevels: Similar format as 'sampleGroups', encoding levels (e.g. order) of the sampleGroups
#' @param dispLevels: Similar format as 'sampleGroups', encoding the display of the groupLevels. Must match 'groupLevels'
#' @param contrasts: Similar format as 'sampleGroups', encoding contrasts in case of one-way ANOVA designs
#' @return A S4-object 'DesignContrast'
#' @examples
#' ## one-way ANOVA
#' parseDesignContrast(sampleGroups="As,Be,As,Be,As,Be",groupLevels="Be,As", dispLevels="Beryllium,Arsenic", contrasts="As-Be")
#' ## design/contrast matrix
#' designFile <- system.file("extdata/example-designMatrix.txt", package="ribiosExpression")
#' contrastFile <- system.file("extdata/example-contrastMatrix.txt", package="ribiosExpression")
#' # minimal information
#' parseDesignContrast(designFile=designFile, contrastFile=contrastFile)
#' # with extra information about sample groups
#' parseDesignContrast(designFile=designFile, contrastFile=contrastFile,sampleGroups="As,Be,As,Be,As,Be",groupLevels="Be,As", dispLevels="Beryllium,Arsenic")
parseDesignContrast <- function(designFile=NULL, contrastFile=NULL,
                                sampleGroups=NULL, groupLevels=NULL, dispLevels=NULL,
                                contrasts=NULL) {
  ## sampleGroups, groupLevels, dispLevels, and contrasts can be either a character string concatenated by commas, or a plain file that encode the strings (one per line)
    sampleGroups <- plainFile2ConcString(sampleGroups)
    groupLevels <- plainFile2ConcString(groupLevels)
    dispLevels <- plainFile2ConcString(dispLevels)
    contrasts <- plainFile2ConcString(contrasts)
    
    if(!is.null(designFile) & !is.null(contrastFile)) {
        return(parseDesignContrastFile(designFile=designFile,
                                       contrastFile=contrastFile,
                                       groupsStr=sampleGroups,
                                       levelStr=groupLevels,
                                       dispLevelStr=dispLevels))
    } else if (!is.null(sampleGroups) & !is.null(contrasts)) {
        return(parseDesignContrastStr(groupsStr=sampleGroups,
                                      levelStr=groupLevels,
                                      dispLevelStr=dispLevels,
                                      contrastStr=contrasts))
    } else {
        stop("Provide either a design matrix and a contrast matrix, or sample groups and contrasts")
    }
}
