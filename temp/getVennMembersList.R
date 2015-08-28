## used in junction with Vennerable package
## export Venn diagram member lists into lists (which can be written then into gmt files or other files)
getVennMembersList <- function(venn) {
    cc <- colnames(venn@IndicatorWeight)
    groups <- cc[1:(length(cc)-1)]

    members <- venn@IntersectionSets
    codes <- names(members)
    codes.split <- strsplit(codes, "")

    tbl <- c("1"="TRUE", "0"=FALSE)
    boolean.codes <- lapply(codes.split, function(x) tbl[x])
    names <- sapply(boolean.codes, function(x) paste(groups, x, sep=":", collapse="; "))
    names(members) <- names
    return(members)
}
writeVennGmt <- function(members, file) {
    write_gmt(members, file, description=sapply(members, length))
}
