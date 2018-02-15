
#' @title 			  Box files (txt) to scidb.mpio.orn.mpg.de
#' @description 	scidb_snbUpdater serializes several functions and performs the update of both the file_status and each b00x table.
#' @param con     a connection object to \code{scidb.mpio.orn.mpg.de} the MySQL database.
#' @param ui      only relevant in an UI interface (e.g. butler() )
#' @param demo	  only relevant when using a demo system (see install_demo_system() )
#' @author  		  MV
#' @export
#' @examples
#' \donotrun{
#'  install_demo_system('mihai', '127.0.0.1')
#'  require(sdb)
#'  con = dbcon(user =  getOption("DB_user") , host = getOption("host"))
#'  scidb_snbUpdater(con)
#'  closeCon(con)
#' }
scidb_snbUpdater <- function(con, p = getOption("path.to.raw_v2"), y = year(Sys.Date()) ,
  db = getOption("snbDB_v2") , ui = FALSE, demo = FALSE) {


  # well_formated_directory
    if(ui) bar <- shiny::Progress$new(min = 1, max = 5)
    if(demo) Sys.sleep(1)
    m1 = 'Searching for proper directory formats ...'
    if(ui) bar$set(1, message = "Preparing DB upload", detail = m1)
  	cat(m1, '\n')

    well_formated_directory(p, y)

  # incoming_files
    if(demo) Sys.sleep(.5)
    m2 = paste('Searching for incoming files (not yet in file_status) on ', p , '...' )
  	if(ui) bar$set(2, message = "Preparing DB upload", detail = m2)
    cat(m2, '\n')

    if(demo) Sys.sleep(.5)

    x = incoming_files(con, p, y, db)

    if( is.null(x)) {
      print( shiny::HTML('There are no new incoming files. file_status table stays unchanged.\n'))
      }

  # file_status_update1
    if(demo) Sys.sleep(.5)
    if( !is.null(x)) {
      m3 = 'Updating file_status table ...'
      if(ui) bar$set(2, message = "Preparing DB upload", detail = m3)
    	cat(m3, '\n')
    	file_status_update1(con, x)
      }

  # hot_files
    if(demo) Sys.sleep(.5)
    m4 = 'Getting the list of new txt files from the file_status table ...'
    if(ui) bar$set(2, message = "Preparing DB upload", detail = m4)
  	cat(m4, '\n')
  	h = hot_files(con, p, db)
    if( nrow(h) == 0) {
      m = 'There are no new files to upload.\n'
      print( shiny::HTML(m))
      stop(m)
      }

  # load_clean_txt
    if(demo) Sys.sleep(.5)
    m5 = 'Fetching and preparing new files. Please wait!'
    if(ui) bar$set(2, message = "Preparing DB upload", detail = m5)
  	cat(m5, '\n')

  	dat = load_clean_txt_v2(h)

    if(ui) bar$close()

  # update_bTables
    if(demo) Sys.sleep(.5)
  	cat('Updating individual box tables')
    if(length(dat))
  	update_bTables(con, dat, ui = ui, demo = demo, db = db)

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
  
  scidb_snbUpdater(con, ...)


 }


