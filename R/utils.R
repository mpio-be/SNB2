

#' @title 		  Box names conversions
#' @description Box names conversions from/to  int, path, basemame to boxname
#' @param 		  x box names (numeric)
#' @return 		  character vector
#' @author  	  MV
#' @export
#' @examples	
#' int2b(1)
#' path2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
#' basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
#' basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX0080.TXT")
#' 
int2b <- function(x) {
  paste0('b',str_pad(x, 3, 'left', pad = '0') )
  }

#' @rdname int2b
#' @export
b2int <- function(x) {
	str_remove(x, 'b') %>%
  as.integer
	}

#' @rdname int2b
#' @export
path2box <- function(x) {
  dirname(x) %>%
    basename %>%
    as.integer
  }

#' @rdname int2b
#' @export
basename2box <- function(ff) {
  basename(ff) %>% 
   str_extract("-?\\d+") %>%
   as.integer
  }




#' @title 		  3 digits box list
#' @description	A list of boxes in westerholz formated as b00x
#' @param 		  x box names (numeric)
#' @return 		  \code{data.table} with column box as character eg. 'b001'
#' @author  	  MV
#' @export
#' @examples	  boxes()
boxes <- function(x = getOption('boxes_v2') ) {
	o = data.table(box = paste0('b',str_pad(x, 3, 'left', pad = '0') ) )
	setkey(o, box)
	o
	}


#' @title 		Extract date
#' @description	Extract date (yyy.mm.dd) from a character string (e.g. a file path)
#' @param 		x character vector(s)
#' @return 		\code{Date} object
#' @author  	MV
#' @export
#' @examples	char2date(x = '2015/2015.01.26/001/box001.txt')
char2date <- function(x) {

	o = str_extract(x, '[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}')
	as.Date(o, format = '%Y.%m.%d')

	}


#' @title     replace NAs
#' @description # helper function to replace NAs in a data.table
#' @export
na.replace <- function(x, replace_value = ''){
  x <- as.matrix(x)
  x[is.na(x)] <- replace_value
  data.table(x)
  }


#' @title     year from file path
#' @description extract year from a file path string
#' @author    MV
#' @export
#' @examples  path2year("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/004/BOX001.TXT")

path2year <- function(x) {
   str_extract(x, "\\b(20)\\d{2}\\b") %>% as.integer
  }



#' @title       Extract date
#' @description Extract date from raw data string
#' @param       x character vector(s)
#' @return      \code{POSIXct} object
#' @author      MV
#' @export
#' @examples    
#' snbstring2date_v2(x = '20170418-171742.202 Transponder: 4B76C4B43A6F0001')
#' snbstring2date_v2(x = '20170418-171742 Transponder: 4B76C4B43A6F0001')
snbstring2date_v2 <- function(x) {

  o = str_extract(x, '(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})\\.(\\d{3})') # ms
  if(  any(is.na(o) ) )
    o = str_extract(x, '(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})') # s

  strptime(o, "%Y%m%d-%H%M%OS") %>% as.POSIXct

  }





#' @title Find if an external software is installed
#' @param nam package name
#' @return logical vector
#' @author MV
#' @export
#' @examples
#' \dontrun{
#' find_installed('secure delete')
#' }
find_installed <- function(nam) {

  o = suppressWarnings( system(paste('which', nam), intern = TRUE) )
  length(o) > 0
  }



# ==========================================================================
#  Undocumented
# ==========================================================================

#' @export
  abbrPath <- function(x) {
    paste(dirname(x) %>% dirname %>% dirname %>% dirname, basename(x), sep = ' .../')
    }

#' @export
  empty2NA <- function(x) {
    if(length(x) == 0) NA else x
    }

#' @export
cclog <- function() {
  paste0(tempdir(), '/cclog.txt')
 }

#' @export
Span <- function(x, label = 'primary', glyphicon = 'open-file', div = TRUE, h = 2) {
  o = paste0('<span class="label label-',label,'"><i class="glyphicon glyphicon-',glyphicon,'"></i>', x, '</span>  ')
  if(div) {
    h = paste0('h', h)
    o = paste0('<div><',h,'>', o, '</',h,'></div>' )
    }
  o
  }

#' @export
options_footer <- function(style = "position: absolute; bottom: 0; left: 1; font-size:12px") {
  div(class = "col-sm-12 text-left text-muted",style=style,
        HTML('<hr>',
            '<strong>Package options:</strong><br>',
            paste('<strong>host:</strong>',        getOption('host'), '<br>'),
            paste('<strong>raw data:</strong>',    paste(getOption('path.to.raw_v2') %>% str_sub(., 1, 18), "..."),'<br>' ),
            paste('<strong>db user:</strong>',     getOption('DB_user') , '<br>'),
            paste('<strong>package:</strong> SNB', packageVersion('SNB2') )
         )
      )

  }



#' @export
check_sys_pwd <- function(password) {
  suppressWarnings( system(paste('echo' ,shQuote(password) ,  '| sudo -S echo 1'),
                intern = TRUE, ignore.stderr = TRUE) ) %>% length == 1
}



#' @export

install_desktop <- function() {

 system('mkdir -p ~/butler/')

 cmd = paste('cp' , paste0(system.file('os_install', package = 'SNB2'), '/*') , '~/butler/')
 system(cmd)

 ff = list.files('~/butler/', full.names = TRUE)

 lapply(ff, Sys.chmod)





}

#' @title force timezones.
#' @param x A POSIXct object
#' @param new.tz time zone to convert to
#' @param diff.tz Optional. The difference between the current and the new timezone, if known. Note that providing this variable will increase the speed of the function 100-fold. Defaults to NULL, in which case the difference is calculated.
#' @details Note if diff.tz is not supplied, this is time-consuming because it uses substring (ca. 1.5 seconds/100000 items). However, if diff.tz is not supplied should handle CET/CEST and other time-zone mixes correctly.
#' @note Note that the time zones that are defined by its offset to UTC are coded e.g. "Etc/GMT-2", where the sign of the code is opposite to the expectation: "Etc/GMT-2" refers to the time zone GMT+2 (which is also printed).
#' @export
force.tz = function(x, new.tz, diff.tz = NULL) {
  if(is.null(diff.tz)) {
  old.tz = attr(x, 'tzone')
  tmp = as.character(as.IDate(x))
  diff.tz = as.numeric(difftime(as.POSIXct(tmp, tz = old.tz), as.POSIXct(tmp, tz = new.tz), units = 'secs'))
  }
  x = x - diff.tz
  attr(x, "tzone") <- new.tz
  x
}
