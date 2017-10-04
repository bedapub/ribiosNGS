#' Get connection to devel database
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite 
#' 
#' @export
#' 
#' @examples
#' devROGeR()
devROGER <- function() {
  DBI::dbConnect(RSQLite::SQLite(), 
                 "/pstore/data/bi/apps/ROGeR/protROGER-v0.1.db")
}
