read_david <- function(file) {
  lines <- readLines(file, skipNul=TRUE)
  lines <- lines[lines!=""]
  clusterHeads <- grep("^Annotation Cluster", lines)
  clusterInd <- cbind(clusterHeads+2, c(clusterHeads[-1]-1, length(lines)))
  clusters <- apply(clusterInd,
                    1, function(x) {lines[x[1]:x[2]]})
  clusters.df <- lapply(clusters, function(x) {
                            con <- textConnection(x)
                            res <- read.table(con, sep="\t", header=FALSE, comment.char="", quote="")
                            close.connection(con)
                            return(res)
                        })
                 
  head <- strsplit(lines[clusterHeads[1]+1], "\t")[[1]]
  head[head=="%"] <- "Percentage"
  head <- make.names(head)
  df <- do.call(rbind, clusters.df)
  colnames(df) <- head
  df$cluster <- factor(rep(seq(along=clusterHeads), sapply(clusters.df, nrow)))
  df <- df[,c("cluster", head)]
  return(df)
}
