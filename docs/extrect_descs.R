library(roxygen2)
library(data.table)

dependencyScoreOf <- function(x, packages.meta) {
  score <- 1
  for(pkg in packages.meta$Package[grepl(x, packages.meta$RibiosDeps)]) {
    score <- score + dependencyScoreOf(pkg, packages.meta)
  }
  score
} 

script.dir <- dirname(sys.frame(1)$ofile)
search.path <- paste(script.dir,"..",sep="/")

dirs <- list.dirs(path = search.path, full.names = FALSE, recursive = FALSE)

desc.files.candidates <- paste(search.path, dirs,sep="/")

print(startsWith(dirs, "ribios"))

filtered.package.dirs <- desc.files.candidates[file.exists(paste(desc.files.candidates,"DESCRIPTION", sep="/")) & startsWith(dirs, "ribios")]

packages.meta <- data.table()

for(package.dir in filtered.package.dirs) {
  package.desc <- roxygen2:::read_pkg_description(package.dir)
  packages.meta <- rbind(packages.meta, package.desc, fill=TRUE)
}

pkg.deps.str <- paste(packages.meta$LinkingTo, packages.meta$Imports, packages.meta$Depends,sep=",")
pkg.deps <- strsplit(gsub("\\s|\\(.*\\)", "", pkg.deps.str), split=",")
pkg.deps.filtered <- lapply(pkg.deps, function(x) x[grepl("ribios",x)])
packages.meta$RibiosDeps <- unlist(lapply(pkg.deps.filtered, function(x) paste(x, collapse = ",")))
packages.meta$Scores <- unlist(lapply(packages.meta$Package, function(x) dependencyScoreOf(x, packages.meta)))

packages.meta.orderd <- packages.meta[order(-Scores)]
cat(sprintf("%s | %s\n", packages.meta.orderd $Package, gsub("[\r\n]", "", packages.meta.orderd $Description)))

cat(paste(sprintf("\"%s\"",packages.meta.orderd$Package),collapse = ","))