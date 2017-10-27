#' Get connection to devel database
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite 
#' 
#' @export
#' 
#' @examples
#' devROGER()
devROGER <- function() {
  DBI::dbConnect(RSQLite::SQLite(), 
                 "/pstore/data/bi/apps/ROGER/protROGER-v0.1.db")
}
