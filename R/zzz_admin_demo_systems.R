## ==========================================================================
# Install DEMO systems locally
# ==========================================================================


#' @title Installs a demo system on 127.0.0.1
#' @description Installs a demo system on 127.0.0.1, prepares a temp RAWDATA folder and changes the options
#' @export
#' @examples
#'\dontrun{
#'install_demo_SNB()
#' }
#'
install_demo_SNB <- function(user='testuser', host ='127.0.0.1', rawdata_root = paste(tempdir(),'SNB_demo_RAWDATA',sep ='/'), db ='SNB_demo') {
    
  #change options & connect to db
    options(DB_user         = user)
    options(host            = host)
    options(path.to.raw_v2  = rawdata_root )
    options(snbDB_v2        = db)

    con = dbcon(user = user, host = host); on.exit(dbDisconnect(con))
    demo_files_loc =system.file('demo', 'txt', package = 'SNB2')
   

  # make demo db
    dbExecute(con, paste('drop database if exists', db ))
    mysqlrestore(system.file('demo', 'db.sql', package = 'SNB2'),db, user, host)

  # populate raw data repo
    rdr = rawdata_root
    if(dir.exists(rdr) ) unlink(rdr, TRUE)
    dir.create(rdr)
    yyyy = paste(rdr, year(Sys.Date()), sep = '/')
    yyyy.mm.dd = paste(yyyy, format(Sys.Date()-7, "%Y.%m.%d"), sep = '/')
    suppressWarnings(dir.create(yyyy.mm.dd, recursive = TRUE))


    o = sapply( list.files(demo_files_loc , full.names = TRUE),function(x) file.copy(x, yyyy.mm.dd ,  recursive = TRUE) )


  # clean and populate test cards when present
     x = cardReader()[, i := 1: .N]

    if(nrow(x) <= length(o) & nrow(x) > 0 ) {
      
      # remove any files on cards
      x[, paste('rm', paste0(mountpoint, '/*') ) %>% system, by =   i ]

      # add new files
      x[, from := list.files(demo_files_loc , full.names = TRUE, recursive = TRUE)[1:.N]]

      x[, file.copy(from, mountpoint) , by = i] 
      
      } else message('SD cards are not available.')
    

  all(o)  

  }



