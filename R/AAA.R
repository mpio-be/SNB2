

#' @title 			  Smart Box Data management
#' @description 	An all-in-one package for data management, user-level functions and hardware feedback.
#' @docType 		  package
#' @name 			    SNB2
#' @usage
#' Overview help files can be found at http://scidb.mpio.orn.mpg.de
#'

.onLoad <- function(libname, pkgname){
	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )

  packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))
  packageStartupMessage('WARNING: date-times in SNBatWESTERHOLZ_v2 are both winter and summer time')

	options(host 		          = 'scidb.mpio.orn.mpg.de')
  options(DB_user           = 'snbAdmin')
  options(path.to.raw_v2    = '/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/')
  options(snbDB_v2          = 'SNBatWESTERHOLZ_v2')
  options(cardReader        = 'Lexar')
  options(cardSize          = 2) #  GB

	options(boxes_v2          =  1:277 )

  options(digits.secs = 3) # ms

	}