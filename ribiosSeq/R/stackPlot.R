#' @export
stackPlot <- function(chr=1, cutoff=400, plot=FALSE, verbose=FALSE,
                      query.start, query.end, input.directory="./counts") {
  has.query.start <- !missing(query.start)
  has.query.end <- !missing(query.end)
  has.query <- has.query.start ||  has.query.end
  
  ## sanity check
  if(has.query.start && has.query.end && query.end < query.start)
    stop("'query.start' must be smaller than 'query.end'")

  progress("Function started", verbose)
  
  chr.no <- chr
  chr.symbol <- sprintf("chr%d", chr.no)
  
  base.files <- dir(input.directory, pattern="*.counts", full.names=TRUE)
  tbl.merge.res <- base.count.group.read(base.files)

  progress("Read file imported", verbose)
  
  tbl.merge <- tbl.merge.res$rle
  tbl.merge.range <- with(tbl.merge.res, Rle(0L, length=as.integer(end-start+1L)))

  tbl.merge.sum <- sumRle(tbl.merge)
  tbl.merge.filter <- tbl.merge.sum > cutoff

  tbl.merge.filtered <- lapply(tbl.merge, function(x) as.integer(x[tbl.merge.filter]))
  tbl.merge.filtered.coord <- which(tbl.merge.filter)+tbl.merge.res$start-1L
  tbl.merge.names <- rep(gsub(".*cyno_([0-9]*[M|F])\\..*", "\\1", base.files),
                         each=sapply(tbl.merge.filtered, length))
    
  tbl.merge.df <- data.frame(coord=rep(tbl.merge.filtered.coord, length(tbl.merge)),
                             count=unlist(tbl.merge.filtered),
                             individual=tbl.merge.names)
  if(verbose) {
    tbl.merge.coord.range <- range(tbl.merge.df$coord)
    progress(sprintf("Coordinates established (%d-%d)", tbl.merge.coord.range[1], tbl.merge.coord.range[2]), verbose=verbose)
  }
  
  if(has.query.start) {
    tbl.merge.df <- subset(tbl.merge.df, coord >= as.integer(query.start))
  }
  if(has.query.end) {
    tbl.merge.df <- subset(tbl.merge.df, coord <= as.integer(query.end))
  }
  
  if(has.query && verbose) {
    tbl.merge.filtered.range <-  range(tbl.merge.df$coord)
    progress(sprintf("Coordinates filtered by query (%d-%d)", tbl.merge.filtered.range[1], tbl.merge.filtered.range[2]), verbose=verbose)
  }
  
  ## wide format
  ind.no <- length(tbl.merge)
  tbl.merge.wide <- reshape(tbl.merge.df, v.names="count", idvar="coord", timevar="individual", direction="wide")
  coord.uniq <- tbl.merge.wide$coord
  tbl.merge.count <- as.matrix(tbl.merge.wide[,1L+(1L:ind.no)])
  if(!plot)
    rm(tbl.merge.df)
  rm(tbl.merge.wide)
  gc()

  ## Feature Requests
  ## Boundary from x to y


  ## detect break points  
  coord.break <- diff(coord.uniq) != 1L
  coord.seg.start <- as.integer(coord.uniq[c(1L, which(coord.break)+1L)])
  coord.seg.end <- as.integer(coord.uniq[c(which(coord.break), length(coord.break))])
  coord.seg.endDistToNextStart <- c(coord.seg.start[2L:length(coord.seg.start)] - coord.seg.end[-length(coord.seg.end)], NA)
  
  progress("Segments detected", verbose)
  
  
  ## calculate read per base per individual, by calculating the mean of rowSums
  coord.seg.breaks <- as.vector(rbind(coord.seg.start, coord.seg.end+1)) ## the end plus one since the right range is not included
  tbl.merge.seg <- cut(coord.uniq, breaks=coord.seg.breaks, include.lowest=TRUE, right=FALSE)
  ## delete non-segment factors
  tbl.merge.seg <- factor(tbl.merge.seg)
  
  tbl.merge.read.perBase <- apply(tbl.merge.count, 2, function(x) tapply(x, tbl.merge.seg, mean))
  progress("Read number per base detected for each individual", verbose)
  
  tbl.merge.read.perBase.perInd <- rowMeans(tbl.merge.read.perBase)
  
  progress("Read number per base per individual detected", verbose)
  
  ## output
  chr.table <- data.frame(read.sum.cutoff=cutoff,
                          seg.start=coord.seg.start,
                          seg.end=coord.seg.end,
                          seg.length=coord.seg.end-coord.seg.start+1,
                          seg.reads.perBase.perIndividual=tbl.merge.read.perBase.perInd,
                          seg.endDistToNextStart=coord.seg.endDistToNextStart)
  chr.table <- cbind(chr.table, tbl.merge.read.perBase)
  output.filename.template <- "%s_segments_statistics_cutoff_%d%s.csv"
  if(!has.query) {
    output.filename <- sprintf(output.filename.template, chr.symbol, cutoff, "")
  } else {
    query.suffix.start <- ifelse(has.query.start, query.start, as.integer(NA))
    query.suffix.end <- ifelse(has.query.end, query.end, as.integer(NA))
    query.suffix <- sprintf("_query_%d_%d",query.suffix.start,query.suffix.end)

    output.filename <- sprintf(output.filename.template, chr.symbol, cutoff,query.suffix)
  }
    
  write.table(chr.table, output.filename,
              row.names=FALSE, sep=";")

  
  ## PDF
  if(plot) {
    pdf(sprintf("stacked_barchart_%s.pdf", chr.symbol), width=200, height=10)
    par.tre <- trellis.par.get()
    par.tre$superpose.polygon$border <- rep("transparent",8)
    par.tre$superpose.symbol$col <- par.tre$superpose.polygon$col
##    coord.format <- formatC(coord.uniq,big.mark="'", format="d")
##    coord.fac <- factor(coord.format, levels=as.character(coord.format))
    chr.skyline <- barchart(count~coord, group=individual, data=tbl.merge.df, horizontal=FALSE,stack=TRUE,
                            main=sprintf("Chromosome %s", chr.no),xlab="Coordinate", ylab="Counts",
                            panel=function(x,y,...) {
                              panel.grid(h=-1, v=0)
                              panel.barchart(x,y,...)
                              start.ind <- match(coord.seg.start, coord.uniq)
                              end.ind <- match(coord.seg.end, coord.uniq)
                              seg.y <- max(y,na.rm=TRUE)*0.3
                              coord.y <- max(y, na.rm=TRUE)*0.4
                              panel.segments(x0=start.ind, y0=rep(0, length(start.ind)),
                                             x1=start.ind, y1=rep(seg.y, length(start.ind)),
                                             col="lightgray", lty=2)
                              ##panel.segments(x0=end.ind, y0=rep(0, length(end.ind)),
                              ##               x1=end.ind, y1=rep(seg.y, length(end.ind)),
                              ##               col="lightgray", lty=2)
                              panel.text(start.ind, coord.y, coord.seg.start, pos=3, col="darkgray", srt=-90)
                              panel.text((start.ind+end.ind)/2,
                                         tbl.merge.read.perBase.perInd, srt=-90,
                                         sprintf("L=%d",coord.seg.end-coord.seg.start+1), col="darkgray")
                              panel.text((start.ind+end.ind)/2,
                                         tbl.merge.read.perBase.perInd, pos=3,srt=-90,
                                         sprintf("Avg.=%1.0f",tbl.merge.read.perBase.perInd), col="darkgray")
                            },
                            auto.key=list(columns=4, points=FALSE, rectangles=TRUE),
                            scales=list(x=list(labels=NULL,tck=c(0,0), rot=45, cex=0.1), y=list(tck=c(1,1), alternating=3)))
    trellis.par.set(par.tre)
    print(chr.skyline)
    dev.off()
    
    rm(chr.skyline)
  }
  
  gc(reset=TRUE)
  invisible(NULL)
}

