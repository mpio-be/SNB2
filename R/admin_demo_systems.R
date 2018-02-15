## ==========================================================================
# Install DEMO systems locally
# ==========================================================================


#' @title Installs a demo system on 127.0.0.1
#' @description Installs a demo system on 127.0.0.1, prepares a temp RAWDATA folder and changes the options
#' @export
#' @examples
#'\dontrun{
#'install_demo_SNB('testuser', '127.0.0.1')
#' }
#'
install_demo_SNB <- function(user, host = '127.0.0.1', rawdata_root = paste(tempdir(), 'SNB_demo_RAWDATA', sep = '/'),
                    db = 'SNB_demo' ) {
    require(sdb)
    con = dbcon(user = user, host = host); on.exit(dbDisconnect(con))

  # set db-s
    dbExecute(con, paste('drop database if exists', db ))
    mysqlrestore(system.file('demo', 'db.sql', package = 'SNB2'),db, user, host)

  # raw txt files v2
    rdr = rawdata_root
    if(dir.exists(rdr) ) unlink(rdr, TRUE)
    dir.create(rdr)
    yyyy = paste0(rdr, year(Sys.Date()) )
    yyyy.mm.dd = paste(yyyy, format(Sys.Date(), "%Y.%m.%d"), sep = '/')

    suppressWarnings(dir.create(yyyy.mm.dd, recursive = TRUE))
    o = sapply(list.files( system.file('demo', 'txt', package = 'SNB2'), full.names = TRUE),
        function(x) file.copy(x, yyyy.mm.dd ,  recursive = TRUE) )

  #change options
    options(DB_user         = user)
    options(host            = host)
    options(path.to.raw_v2  = rawdata_root )
    options(snbDB_v2        = db)

  all(o)  

  }



