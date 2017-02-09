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
setMethod("nContrast", "DesignContrast", function(object) {
              return(ncol(object@contrasts))
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

## check consistency between signal matrix and design matrix
isInputDesignConsistent <- function(descon, sampleNames) {
    designSampleNames <- colnames(designMatrix(descon))
    if(setequal(sampleNames, designSampleNames)) {
        if(identical(sampleNames, designSampleNames)) {
            return(invisible(TRUE))
        } else {
            warning("The order of samples in the design matrix differ from that in the input matrix! Please verify the consistency!")
            return(invisible(FALSE))
        }
    } else {
        warning("Sample names in the input matrix and in the design matrix do not match")
        return(invisible(FALSE))
    }
}


#' Parse study design and asked questions encoded in design and contrast matrices or in one-way ANOVA designs
#' @param designFile: A plain tab-delimited file with headers encoding the design matrix, or NULL
#' @param contrastFile: A plain tab-delimited file with headers encoding the contrast matrix, or NULL
#' @param sampleGroups: A character string concatenated by commas (e.g. A,B,C), or a plain text file containing one string per line (e.g. A\emph{newline}B\emph{newline}C), encoding sample group memberships.
#' @param groupLevels: Similar format as 'sampleGroups', encoding levels (e.g. order) of the sampleGroups
#' @param dispLevels: Similar format as 'sampleGroups', encoding the display of the groupLevels. Must match 'groupLevels'
#' @param contrasts: Similar format as 'sampleGroups', encoding contrasts in case of one-way ANOVA designs
#' @param expSampleNames: A vector of character strings giving the expected sample names (e.g. those in the input matrix)
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
                                contrasts=NULL, expSampleNames=NULL) {
  ## sampleGroups, groupLevels, dispLevels, and contrasts can be either a character string concatenated by commas, or a plain file that encode the strings (one per line)
    sampleGroups <- plainFile2ConcString(sampleGroups)
    groupLevels <- plainFile2ConcString(groupLevels)
    dispLevels <- plainFile2ConcString(dispLevels)
    contrasts <- plainFile2ConcString(contrasts)
    
    if(!is.null(designFile) & !is.null(contrastFile)) {
        descon <- parseDesignContrastFile(designFile=designFile,
                                       contrastFile=contrastFile,
                                       groupsStr=sampleGroups,
                                       levelStr=groupLevels,
                                       dispLevelStr=dispLevels)
    } else if (!is.null(sampleGroups) & !is.null(contrasts)) {
        descon <- parseDesignContrastStr(groupsStr=sampleGroups,
                                      levelStr=groupLevels,
                                      dispLevelStr=dispLevels,
                                      contrastStr=contrasts)
    } else {
        stop("Provide either a design matrix and a contrast matrix, or sample groups and contrasts")
    }

    if(!is.null(expSampleNames)) {
        isInputDesignConsistent(descon, expSampleNames)
    }
    return(descon)
}

.contrastSampleIndices<- function(descon, contrast) {
    contrastMat <- contrastMatrix(descon)
    designMat <- designMatrix(descon)
    haltifnot(contrast %in% colnames(contrastMat) || contrast %in% 1:ncol(contrastMat),
              msg=sprintf("contrast '%s' not found in the contrast matrix",
                  contrast))
    currContrast <- contrastMat[, contrast]
    isNonZero <- currContrast!=0
    ascNonZero <- which(isNonZero)[order(currContrast[isNonZero], decreasing=FALSE)]
    ascInds <- lapply(ascNonZero, function(designInd)
        which(designMat[,designInd]!=0))
    ## if there is intercept AND the sum of current contrast is not zero, add samples only having intercept
    isInter <- apply(designMat, 2L, function(x) all(x==1))
    if(any(isInter) & sum(currContrast)!=0) {
        hasInterOnly <- which(apply(designMat, 1L, function(x) all(x[!isInter]==0)))
        ascInds <- c(list(intersect=hasInterOnly), ascInds)
    }
    res <- unname(unlist(ascInds))
    return(res)
}

#' Return indices of samples involved in the given contrast of two or more coefficients
#' @param object: A \code{DesignContrast} object
#' @param contrast: Either a contrast name or a integer indicating the index of the contrast
#' @return An integer vector, indices of samples that are involved, sorted by the ascending order of the coefficients of the contrast
#' @examples
#' ## one-way ANOVA
#' (myDesCon <- parseDesignContrast(sampleGroups="As,Be,As,Be,As,Be",groupLevels="Be,As", dispLevels="Beryllium,Arsenic", contrasts="As-Be"))
#' contrastSampleIndices(myDesCon, 1L)
#' (myInterDesCon <- new("DesignContrast", design=matrix(c(rep(1,6), rep(0,2), rep(1,2), rep(0,2), rep(0,4), rep(1,2)), nrow=6, byrow=FALSE), contrasts=matrix(c(0,1,0, 0,0,1, 0,-1,1), byrow=FALSE, nrow=3), groups=factor(rep(c("As", "Be", "Cd"), each=2)), dispLevels=c("Arsenic", "Beryllium", "Cadmium")))
#' (cont1Ind <- contrastSampleIndices(myInterDesCon, 1L))
#' (cont2Ind <- contrastSampleIndices(myInterDesCon, 2L))
#' (cont3Ind <- contrastSampleIndices(myInterDesCon, 3L))
#' stopifnot(identical(cont1Ind, 1:4))
#' stopifnot(identical(cont2Ind, c(1:2, 5:6)))
#' stopifnot(identical(cont3Ind, c(3:6)))
setMethod("contrastSampleIndices", c("DesignContrast", "character"), function(object, contrast) {
              .contrastSampleIndices(object, contrast)
          })
setMethod("contrastSampleIndices", c("DesignContrast", "numeric"), function(object, contrast) {
              .contrastSampleIndices(object, contrast)
          })

