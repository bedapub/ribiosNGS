gmt2dataframe <- function(file) {
    glist <- read_gmt_list(file)
    ggenes <- lapply(glist, function(x) x$genes)
    df <- data.frame(geneset=rep(names(glist),sapply(ggenes, length)),
                     gene=unlist(ggenes), row.names=NULL)
    return(df)
}
