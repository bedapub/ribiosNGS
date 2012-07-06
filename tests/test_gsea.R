library(ribiosExpression)

up.file <- system.file("extdata/tags_up.grp", package="ribiosExpression")
down.file <- system.file("extdata/tags_down.grp", package="ribiosExpression")

grp2gmt(readLines(up.file, n=-1))
options(error=recover)
grp2gmt(readLines(up.file, n=-1), chiptype="HG_U95AV2")
grp2gmt(readLines(up.file, n=3), chiptype="HG_U95AV2", name="my_up")
grp2gmt(readLines(down.file, n=-1))
grp2gmt(readLines(down.file, n=-1), chiptype="HG_U95AV2")
grp2gmt(readLines(down.file, n=-1), chiptype="HG_U95AV2")

grpFiles2gmt(up.file)
grpFiles2gmt(c(up.file, down.file))
grpFiles2gmt(c(up.file, down.file), n=3)
grpFiles2gmt(c(up.file, down.file), n=3, chiptype="HG_U95AV2")
options(error=NULL)
