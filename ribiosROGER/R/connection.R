#' Get connection to devel database
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite 
#'
#' @param dest SQLite3 file which holds the ROGER database. The default value is the prototype database.
#' 
#' @export
#' 
#' @examples
#' devROGER()
devROGER <- function(dest="/pstore/data/bi/apps/ROGER/protROGER-v0.1.db") {
  DBI::dbConnect(RSQLite::SQLite(), 
                 dest)
}
