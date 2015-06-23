## Functions to read coordinate-count tables, for example those produced in the SAGE experiments
## jitao_david.zhang@roche.com

checkFilesExist <- function(files) {
  if(length(files)==0) {
    warning("No file defined\n")
    return(FALSE)
  }
  file.exists <- file.exists(files)
  if(all(file.exists)) {
    return(TRUE)
  } else {
    warning("Following files do not exist:%s", paste(files[!file.exists], collapse=","))
    return(FALSE)
  }
}
readCoordCountFile <- function(file) {
  if(!checkFilesExist(file))
    stop("File do not exist")
  res <- .Call("bios_read_coord_count_file", file)
  colnames(res) <- c("coord", "count")
  return(res)
}

readCoordCountFileUsingR <- function(file, head=FALSE, diff.only=FALSE,...) {
  tbl <- read.table(file, sep="\t", head=head,
                    col.names=c("coord", "count"),
                    colClasses=c("integer", "integer"),
                    ...)
  
  tbl <- tbl[order(tbl$coord, decreasing=FALSE),]
  if(diff.only) {
    count.diff <- with(tbl, which(diff(count)!=0))
    count.diff.sel <- c(1, count.diff, nrow(tbl))
    tbl <- tbl[count.diff.sel,]
  }
  res <- list(coord=Rle(tbl$coord), count=Rle(tbl$count))
  rm(tbl)
  return(res)
}

readCoordCountRange <- function(inputDir, pattern="\\.counts$") {
 files <- dir(inputDir,
               pattern=pattern, full.names=TRUE)
  if(!checkFilesExist(files))
    stop("No input files found, check input directory or the file name patterns\n")

  ranges <-  vector("list", length=length(files))
  for(i in seq(along=files)) {
    tbl.curr <- .Call("bios_read_coord_count_file", files[i])

    tbl.cord.diff <- diff(tbl.curr[,1])
    tbl.count.diff <- diff(tbl.curr[,2])
    tbl.isNewSeg <- tbl.cord.diff != 1 | tbl.count.diff != 0
    
    tbl.start <- c(1, which(tbl.isNewSeg)+1)
    tbl.end <-  c(which(tbl.isNewSeg), nrow(tbl.curr))

    currIRange <- IRanges(start=tbl.curr[tbl.start,1L],
                          end=tbl.curr[tbl.end,1L])
    currRangedData <- RangedData(currIRange, count=tbl.curr[tbl.start,2])
    ranges[[i]] <- currRangedData
  }
 res <- RangedDataList(ranges)
 return(res)
}

## RangedDataList:
## option 1: pmin larger than a threshold in X samples
## option 2: mean larger than a threshold
## option 3: pmax larger than a threshold



readCoordCounts <- function(inputDir, pattern="\\.counts$") {
  files <- dir(inputDir,
               pattern=pattern, full.names=TRUE)
  if(!checkFilesExist(files))
    stop("No input files found, check input directory or the file name patterns\n")

  tbl <- vector("list", length=length(files))
  coord.max <- 0
  coord.min <- Inf
  for(i in seq(along=tbl)) {
    tbl.curr <- .Call("bios_read_coord_count_file", files[i])

    tbl.curr <- tbl.curr[order(tbl.curr[,1L], decreasing=FALSE),]
    tbl.curr.maxCoord <- max(tbl.curr[,1L], na.rm=TRUE)
    tbl.curr.minCoord <- min(tbl.curr[,1L], na.rm=TRUE)
    if( tbl.curr.maxCoord > coord.max) coord.max <- tbl.curr.maxCoord
    if( tbl.curr.minCoord < coord.min) coord.min <- tbl.curr.minCoord
    tbl[[i]] <- list(coord=tbl.curr[,1L], count=Rle(tbl.curr[,2L]))
  }
  rm(tbl.curr)

  ## to avoid copying large tables, here we use a loop
  rle.lists <- vector("list", length=length(tbl))
  for(i in seq(along=files)) {
    res <- Rle(0L, lengths = coord.max - coord.min + 1)
    ind <- as.integer(tbl[[i]]$coord - Rle(coord.min-1L, lengths=length(tbl[[i]]$coord)))
    res[ind] <- tbl[[i]]$count
    rle.lists[[i]] <- res
    rm(res)
  }
  rle.list <- RleList(rle.lists)
  gc()
  return(list(rle=rle.list, start=coord.min, end=coord.max))
}

writeCoordCounts <- function(cclist, file) {
  ## write the table
  browser()
}

## Rle manipulations
sumRle <- function(rleList) { ## ask for equal length Rles
  res <- rleList[[1]]
  for(i in 2:length(rleList)) {
    res <- rleList[[i]] + res
  }
  return(res)
}

diffRle <- function(df, start, end) {
  res <- Rle(0L, lengths=end-start+1)
  ind <- df$coord - Rle(as.integer(start)-1L, length(df$coord))
  ## memory intensive step, can we debug?
  res[as.integer(ind)] <- as.integer(df$count)
  gc()
  return(res)
}
