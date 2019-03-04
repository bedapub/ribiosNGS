#' Decode password with function implemented with pwencode
#' 
#' Decode password encypted with pwencode.
#' 
#' See pwdecode function documentation in BIOS for implemetnation details.
#' 
#' Note that since R does not support strings embedding null values
#' (\code{\000}), the password to be decoded has to be given with two slashes,
#' e.g. \sQuote{ \\001\\000\\129\\235}.
#' 
#' @param password Character string to be decoded. If starting with a empty
#' character, the string is sent for decoding; otherwise, it is deemed as clear
#' text password and returned.
#' @return Decoded character string, or empty string if decoding fails
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>. The C library code
#' was written by Detlef Wolf.
#' @references BIOS C module list,
#' \url{http://bioinfo.bas.roche.com:8080/bios/common/modulelist.html}
#' @examples
#' 
#' mycode <- " \\001\\000\\141\\314\\033\\033\\033\\033\\033\\142\\303\\056\\166\\311\\037\\042"
#' pwdecode(mycode)
#' 
#' @export pwdecode
pwdecode <- function(password) {
  if(!grepl("^ ", password)) {
    return(password)
  }
  num <- strsplit(password, "\\\\")[[1]][-1]
  x <- as.raw(strtoi(num, base=8L))
  .Call(C_pwdecode,x, PACKAGE="ribiosUtils")
}

#' Encode a password
#' @param label label used to encode the password
#' @param key password key
#' @export pwencode
pwencode <- function(label="VAR", key) {
  if(missing(key))
    stop("'key' must not be missing")
  res <- system(sprintf("/pstore/apps/bioinfo/bin/pwencode %s %s", label, key), intern=TRUE)
  return(res)
}
