refactor <- function(factor, levels) {
  if(!is.factor(factor))
    stop("'factor' must be factor\n")
  if(!nlevels(factor)==length(levels))
    stop("Level number of factor' must be of the same length of 'levels'\n")
  if(is.null(names(levels)))
    stop("'levels' must be a named vector: names are (ordered) old levels, values are new levels")
  current.levels <- levels(factor)
  oldlevels <- names(levels)
  newlevels <- unname(levels)
  if(!all(oldlevels %in% current.levels)) {
    missing.levels <- setdiff(oldlevels,current.levels)
    stop(paste("Following old levels are not found:\n",
               paste(missing.levels, collapse=" "),"\n"))
  }
  if(!all(current.levels %in% oldlevels)) {
    missing.levels <- setdiff(current.levels, oldlevels)
    stop(paste("Following current levels are not included in 'levels':\n",
               paste(missing.levels, collapse=" "), "\n"))
  }
  factor.new <- factor(factor, levels=oldlevels)
  levels(factor.new) <- newlevels
  return(factor.new)
}

relevels <- function(x, refs) {
  if(!all(refs %in% levels(x))) {
    missing <- which(!(refs %in% levels(x)))
    stop("The following levels are not found in x:\n",paste(refs[missing], sep=","))
  }
  refs <- rev(refs)
  for (i in refs) {
    x <- relevel(x, ref=i)
  }
  return(x)
}

ofactor <- function(x,...) factor(x, levels=unique(as.character(x)),...)

##test.relevels <- function() {
##  cup <- c("HSV","FCBayern","KSC","VfB")
##  teams <- factor(cup)
##  orderTeams <- relevels(teams, cup)
##
##  checkEquals(levels(orderTeams), cup)
##  checkException(relvels(teams, c(cup, "SF")))
##}

cutInterval <- function(x, step=1,
                        labelOption=c("cut.default", "left", "right"),
                        include.lowest=FALSE, right=TRUE, dig.lab=3, ordered_result=FALSE,...) {
  labelOption <- match.arg(labelOption,
                     c("left", "right", "cut.default"))
  x.max <- max(x, na.rm=TRUE)
  x.min <- min(x, na.rm=TRUE)
  cut.up <- ifelse(x.max %% step==0,
                   x.max %/% step, x.max %/%step+1)*step
  cut.low <- ifelse(x.min %/% step==0,
                    0, step * (x.min %/% step))
  cut.scale <- seq(from=cut.low, to=cut.up, by=step)
  labels <- NULL
  if(labelOption=="left") {
    labels <- cut.scale[-length(cut.scale)]
  } else if (labelOption=="right") {
    labels <- cut.scale[-1]
  }
  x.cut <- cut(x, cut.scale,labels=labels,
               include.lowest=include.lowest, right=right, dig.lab=dig.lab, ordered_result=ordered_result, ## default in cut
               ...)
  return(x.cut)
}

refactorNum <- function(x, decreasing=FALSE) {
  x <- factor(as.character(x))
  new.levels <- sort(as.numeric(levels(x)),
                     decreasing=decreasing)
  factor(x, levels=new.levels)
}
