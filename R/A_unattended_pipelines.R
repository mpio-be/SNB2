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
scidb_snbUpdater <- function(file = '~/scidb_snbUpdater.log') {

    if(file.exists(file)) file.remove(file)

    cat(' ------> Started at:', format(Sys.time(), "%a %b %d %X %Y %Z") , '\n', append=TRUE, file= file)     
        

    o = scidb_snbUpdater.b000()
    
    Sys.sleep(5)

    scidb_snbUpdater.transponders()



}