

#' @import methods tools data.table foreach shinydashboard  shiny shinytoastr
NULL

#' @title 			  Smart Box Data management
#' @description 	An all-in-one package for data management, user-level functions and hardware feedback.
#' @docType 		  package
#' @name 			    SNB2
#' @export
#' @note
#' Overview help files can be found at http://scidb.mpio.orn.mpg.de
#'

.onLoad <- function(libname, pkgname){


	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )

  # Do I run on scidbadmin?
  if( .Platform$OS.type !=  "unix" ) db_host = '127.0.0.1'

  if( .Platform$OS.type ==  "unix" ) {

    is.scidbadmin = grepl("192.168.2.124", system('hostname -I', intern = TRUE) )
    # Set DB host
    db_host = if(is.scidbadmin) 'scidb.mpio.orn.mpg.de' else '127.0.0.1'
 
    }

  options(host              = db_host )
  options(DB_user           = 'snbAdmin')
  options(path.to.raw_v2    = '/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/')
  options(path.to.raw_v2_SF = '/ds/raw_data_kemp/FIELD/Westerholz/SF/RAWDATA/')
  options(snbDB_v2          = 'SNBatWESTERHOLZ_v3')
  options(transpondersDB    = 'BTatWESTERHOLZ')
  options(cardReader        = 'Lexar')
  options(cardSize          = 2) 
	options(boxes_v2          =  1:277 )
  options(digits.secs       = 3)

	}


