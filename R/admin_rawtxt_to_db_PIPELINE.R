
#' @title 			  Box files (txt) to scidb.mpio.orn.mpg.de
#' @description 	scidb_snbUpdater serializes several functions and performs the 
#'                update of both the file_status and each b00x table.
#' @param u       user, default to getOption("DB_user")
#' @param h       host, default to getOption("host")
#' @param p       path, default to getOption("path.to.raw_v2")
#' @param y       year, default to year(Sys.Date()
#' @param db      db name, default to getOption("snbDB_v2")
#' @return        TRUE if update_bTables() is called and successful 
#' @author  		  MV
#' @export
#' @examples
#' \donotrun{
#'  require(SNB2)
#'  install_demo_SNB('mihai', '127.0.0.1')
#'  scidb_snbUpdater()
#' }
#' 
scidb_snbUpdater <- function(u =  getOption("DB_user") , h = getOption("host"), 
    p = getOption("path.to.raw_v2"), y = year(Sys.Date()) , db = getOption("snbDB_v2") ) {
    con = dbcon(user = u , host = h); on.exit( closeCon(con) )

  # well_formated_directory
  	cat(' ------> Searching for proper directory formats ...')
    well_formated_directory(p, y); cat('done\n')

  # incoming_files
   cat(paste(' ------> Searching for incoming files (not yet in file_status) on ', p , '...' ), '\n')

    x = incoming_files(con, p, y, db)

    if( is.null(x)) {
      cat('     There are no new incoming files. \n       file_status table stays unchanged.\n')
      }

  # file_status_update1
    if( !is.null(x)) {
    	cat(' ------> Updating file_status table ...', '\n')
    	file_status_update1(con, x)
      }

  # hot_files
  	cat(' ------> Getting the list of new txt files from the file_status table ...', '\n')
  	h = hot_files(con, p, db)
    if( nrow(h) == 0) {
      
      cat('   There are no new files to upload. Will stop now.\n')
      stop('There are no new files to upload.')

      }

  # load_clean_txt
    cat(' ------> Fetching and preparing new files. This can take a while.... \n')
  	

  	dat = load_clean_txt_v2(h)


  # update_bTables
  	cat(' ------> Updating individual box tables ....  \n ')
    if(length(dat))
  	o = update_bTables(con, dat, db = db)

    return(o)


    cat(' ------ DONE ------ \n ')

 }


#' @title        Unload and reload files
#' @description  Unload and reload files by ID
#' @param con    a connection object
#' @param id     id
#' @param ...    goes to  scidb_snbUpdater
#' @export
#' 
drop_and_reload = function(con, id, ...) {
  dropped_files = drop_by_id( con, id )
  
  scidb_snbUpdater()


 }


