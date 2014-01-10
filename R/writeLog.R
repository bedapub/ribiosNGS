writeLog <- function(..., con=stdout(), level=0) {
  format <- paste("[%s] ",
                  paste(rep(" ", level), collapse=""),
                  "%s", sep="")
  text <- sprintf(format,
                  format(Sys.time(),"%y-%m-%d %X"),
                  paste(list(...), collapse=" "))      
  writeLines(text, con=con)
}
