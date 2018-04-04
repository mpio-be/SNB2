#' @title         Unattended pipelines
#' @return        NULL
#' @author        MV
#' @export
#' @examples
#' \dontrun{
#'  require(SNB2)
#'  
#'  demo_setup(install.test.db = TRUE, admin.user = 'mihai')
#'  scidb_snbUpdater()
#' }
#' 
#' 
scidb_snbUpdater <- function() {

    o = scidb_snbUpdater.b000()
    if(o > 0)
        scidb_snbUpdater.transponders ()



}