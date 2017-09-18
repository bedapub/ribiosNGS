#' Get connection to devel database
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite 
#' 
#' @export
#' 
#' @examples
#' devROGeR()
devROGeR <- function() {
  DBI::dbConnect(RSQLite::SQLite(), "/pstore/data/bi/apps/ROGeR/protROGeR-v0.1.db")
}
