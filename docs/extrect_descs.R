library(roxygen2)

dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

desc.files.candidates <- paste(dirs,"DESCRIPTION",sep="/")

filtered.package.dirs <- dirs[file.exists(desc.files.candidates)]


for(package.dir in filtered.package.dirs) {
  package.desc <- roxygen2:::read_pkg_description(package.dir)
  cat(sprintf("%s | %s\n", package.desc$Package, gsub("[\r\n]", "", package.desc$Description)))
}
