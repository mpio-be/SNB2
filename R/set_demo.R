
#' @title Installs a demo system 
#' @description Installs a demo system: prepares a temp RAWDATA folder and changes the options
#' @param  user user
#' @param  admin.user admin.user
#' @param  host host
#' @param  rawdata_root rawdata_root
#' @param  db db
#' @param  install.test.db  when TRUE (the default) installs a test db
#' @export
#' @examples
#'\dontrun{
#'demo_setup(install.test.db = TRUE)
#' }
#'
demo_setup <- function(user = 'testuser', admin.user, host ='127.0.0.1', 
    rawdata_root = paste(tempdir(),'SNB_demo_RAWDATA',sep ='/'), db = 'tests',
    install.test.db = TRUE ) {
  
  # restore db
  if( install.test.db) {
  mysqlrestore(system.file('testdb.sql', package = 'SNB2'), user = admin.user, db = db )
    
  }


  #change options & connect to db
    options(DB_user         = user)
    options(host            = host)
    options(path.to.raw_v2  = rawdata_root )
    options(snbDB_v2        = db)
    options(boxes_v2        = 81:85) # in test_files_SNB directory

    demo_files_loc = system.file('test_files_SNB', package = 'SNB2')

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
