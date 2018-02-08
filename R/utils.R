
#' @title        Extract transponder
#' @description  regexp a transponder
#' @param        x character vector
#' @return       extracted character vector
#' @author       MV
#' @examples
#' transp_extract(x = c('280196A801AF0001', '28A0000000008001', 'A1422A8758200001')   )
#' d = dbq(q = 'select * from COMMON.TRANSPONDERS_LIST', user = 'mihai')
#' d[, check := transp_extract(transponder)]
#' d[transponder != check]
transp_extract <-  function(x) {
 str_extract(x, "\\w{11}F0001|A1\\w{9}00001|\\w{3}0000000008001")
 }



#' @title 		fast_POSIXct
#' @description Quickly converts local timestamps to a POSIXct vector
#' @param		x	timestamps (YYYY-mm-dd HH:MM:SS)
#' @param		tz	local timezone
#' @return		POSIXct vector
fast_POSIXct <- function(x, tz = 'CEST') {
	stopifnot(is.character(x))
	GMT <- fasttime::fastPOSIXct(x, tz='GMT')
	epoch <- as.numeric(GMT)
	t0 <- '1970-01-01'
	z <- as.POSIXct(epoch, tz=tz, origin=t0)
	adjusted <- z - as.POSIXlt(z)$isdst * 3600
	return(adjusted)
	}

#' @title 		integer to box name
#' @description convert an integer as b00x
#' @param 		x box names (numeric)
#' @return 		character vector
#' @author  	MV
#' @examples	int2b(1)
int2b <- function(x) {
	paste0('b',str_pad(x, 3, 'left', pad = '0') )
	}

#' @title 		  3 digits box list
#' @description	A list of boxes in westerholz formated as b00x
#' @param 		  x box names (numeric)
#' @return 		  \code{data.table} with column box as character eg. 'b001'
#' @author  	  MV
#' @examples	  boxes()
boxes <- function(x = 1:277) {

	o = data.table(box = paste0('b',str_pad(x, 3, 'left', pad = '0') ) )
	setkey(o, box)
	o


	}


#' @title 		Extract date
#' @description	Extract date (yyy.mm.dd) from a character string (e.g. a file path)
#' @param 		x character vector(s)
#' @return 		\code{Date} object
#' @author  	MV
#' @examples	char2date(x = '2015/2015.01.26/001/box001.txt')
char2date <- function(x) {

	o = str_extract(x, '[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}')
	as.Date(o, format = '%Y.%m.%d')

	}


# helper function to replace NAs in a data.table
na.replace <- function(x, replace_value = ''){
  x <- as.matrix(x)
  x[is.na(x)] <- replace_value
  data.table(x)
  }

#' @title     Read raw
#' @description Read raw box txt file without conversion and string splitting
#' @param     filePath  file path to the box file
#' @return    a \code{data.table} with two columns: V (the data string )  and bout_length (N time the string is repeated)
#' @author    MV
#' @examples
#' readRaw('/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/004/BOX001.TXT')
#' # without embedded null string
#' readRaw("/ds/raw_data_kemp/FIELD/Westerholz/SF/RAWDATA/2014/2014.11.20/014/BOX001.TXT")


readRaw <- function(filePath)  {
  x = try(fread(filePath, header = FALSE, colClasses = 'character'), silent = TRUE)

  if(inherits(x, 'try-error') ) { # expect embeded nul
    rm(x)
    tt =tempfile()
    system(paste0("tr < ", filePath, " -d '\\000' >", tt))
    x =  fread(tt, header = FALSE, colClasses = 'character')
  }

  x = rle(x[, V1])
  x = data.table(V = x$values, bout_length = x$lengths)
  x

  }

#' @title     snb string to datetime_
#' @description convert an SNB string to a datetime object
#' @param     x  a character vector
#' @return    a character vector yyyy-dd-mm hh:mm:ss
#' @author    MV
#' @examples  rawstring2datetime('0212151805550010') # old format

rawstring2datetime <- function(x) {

    str_c(
      str_c(
        str_c(20,str_sub(x, 5, 6) ) ,  # year (YYYY)
        str_sub(x, 3, 4),               # month
        str_sub(x, 1, 2), sep = '-'),  # day
      str_c(
        str_sub(x, 7, 8),               # hour
        str_sub(x, 9, 10),               # min
        str_sub(x, 11, 12), sep = ':'),sep = " ") # sec

  }

#' @title     year from file path
#' @description extract year from a file path string
#' @author    MV
#' @examples  path2year("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/004/BOX001.TXT")

path2year <- function(x) {

   str_extract(x, "\\b(20)\\d{2}\\b") %>% as.integer

  }

#' @title     box from file path (v1)
#' @description extract box from a file path string
#' @author    MV
#' @examples  path2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX001.TXT")

path2box <- function(x) {
   # str_extract(x, "\\b\\d{3}\\b")
  dirname(x) %>%
    basename %>%
    as.integer

  }

#' @title     test transponders
#' @description the latest test transponder and their holders (authors)
#' @param     con a db connection
#' @return    a data.table
#' @author    MV
#' @examples  con = dbcon('mihai', host = 'scidb.mpio.orn.mpg.de')
#' tetr(con)


#' @title       basename to box
#' @description extract box from a file path (basename) string
#' @author    MV
#' @examples  basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
#' basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX0080.TXT")

basename2box <- function(ff) {

  as.integer( str_extract(basename(ff) , "-?\\d+") )

  }





abbrPath <- function(x) {
  paste(dirname(x) %>% dirname %>% dirname %>% dirname, basename(x), sep = ' .../')
  }

#' @title Find if an external software is installed
#' @param nam package name
#' @return logical vector
#' @author MV
#' @examples
#' find_installed('secure delete')
find_installed <- function(nam) {

  o = suppressWarnings( system(paste('which', nam), intern = TRUE) )
  length(o) > 0
  }

#' @title Installs a demo system on 127.0.0.1
#' @description Installs a demo system on 127.0.0.1, prepares a temp RAWDATA folder and changes the options
#' @examples
#'\dontrun{
#'install_demo_system('mihai', '127.0.0.1')
#' }
#'
install_demo_system <- function(user, host = '127.0.0.1', rawdata_root = paste(tempdir(), 'snb_demo_RAWDATA', sep = '/'),
                    db = 'demo' ) {

    # set db-s
    dbq(user = user, host = host, q = paste('drop database if exists', db))
    dbq(user = user, host = host, q = paste('drop database if exists', paste0(db, '_v2') ))
    mysqlrestore(system.file('demo', 'db.sql', package = 'SNB'),db, user, host)
    mysqlrestore(system.file('demo_v2', 'db.sql', package = 'SNB'),paste0(db, '_v2'), user, host)

  # raw txt files v1
    rdr = paste0(rawdata_root, '/')
    if(dir.exists(rdr) ) unlink(rdr, TRUE)
    dir.create(rdr)
    yyyy = paste0(rdr, year(Sys.Date()) )
    yyyy.mm.dd = paste(yyyy, format(Sys.Date(), "%Y.%m.%d"), sep = '/')

    dir.create(yyyy.mm.dd, recursive = TRUE)
    sapply(list.files( system.file('demo', 'txt', package = 'SNB'), full.names = TRUE),
        function(x) file.copy(x, yyyy.mm.dd ,  recursive = TRUE) )


  # raw txt files v2
    rdr = paste0(rawdata_root, '_v2/')
    if(dir.exists(rdr) ) unlink(rdr, TRUE)
    dir.create(rdr)
    yyyy = paste0(rdr, year(Sys.Date()) )
    yyyy.mm.dd = paste(yyyy, format(Sys.Date(), "%Y.%m.%d"), sep = '/')

    dir.create(yyyy.mm.dd, recursive = TRUE)
    sapply(list.files( system.file('demo_v2', 'txt', package = 'SNB'), full.names = TRUE),
        function(x) file.copy(x, yyyy.mm.dd ,  recursive = TRUE) )

  #change options
    options(DB_user         = user)
    options(host            = host)
    options(path.to.raw     = paste0(rawdata_root, '/') )
    options(path.to.raw_v2  = paste0(rawdata_root, '_v2/') )
    options(snbDB           = db)
    options(snbDB_v2        = paste0(db, '_v2'))

  }


empty2NA <- function(x) {
  if(length(x) == 0) NA else x
}


#' @title       List all data directories
#' @param p     path to raw data, default to getOption("path.to.raw")
#' @return      a data.table
#' @author      MV
#'
data_dirs <- function(p = getOption("path.to.raw") ) {
  x = list.dirs(p, recursive = FALSE ) %>% sapply(., list.dirs, recursive = FALSE)
  x = data.table(path = unlist(x))
  x[, dir := basename(path)]
  x[, year := dirname(path) %>% basename]
  x
  }



