

#' Plot raw data in the field
#'
#' @param path The path to the BOX001.TXT file. Defaults to the desktop (/home/bt/Desktop/)
#' @param box A box number (defaults to 999)
#' @return An actogram (.pdf) that is located at the specified path and a data table with the underlying data.
#' @export 
#' @examples
#' #copy a BOX001.TXT to the desktop if the blue tit user and run
#' x = plot_from_raw()
#' x
#' #have a look at your desktop. A file called box_999.pdf should have appeared.
plot_from_raw = function(path = "/home/bt/Desktop/", box = 999) {
  h = data.table(box = box, path = paste0(path, "BOX001.TXT"), id = 0)
  x = load_clean_txt(h)[[1]]
  x[, datetime_ := as.numeric(as.POSIXct(datetime_))]
  x[, box := box]
  x = events(x)
  x[, ID := transp]
  
  (actogramPlot(x))
  ggsave(paste0(gsub("BOX001.TXT", '', path), "box_", box, ".pdf"), device = 'pdf')

  return(x)
}


#' @title 		integer to box name
#' @description convert an integer as b00x
#' @param 		x box names (numeric)
#' @return 		character vector
#' @author  	MV
#' @export
#' @examples	int2b(1)
int2b <- function(x) {
	paste0('b',str_pad(x, 3, 'left', pad = '0') )
	}

#' @title 		  3 digits box list
#' @description	A list of boxes in westerholz formated as b00x
#' @param 		  x box names (numeric)
#' @return 		  \code{data.table} with column box as character eg. 'b001'
#' @author  	  MV
#' @export
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

#' @title     box from file path (v1)
#' @description extract box from a file path string
#' @export
#' @author    MV
#' @examples  path2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX001.TXT")

path2box <- function(x) {
   # str_extract(x, "\\b\\d{3}\\b")
  dirname(x) %>%
    basename %>%
    as.integer

  }



#' @title       basename to box
#' @description extract box from a file path (basename) string
#' @author    MV
#' @export
#' @examples  
#' basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
#' basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX0080.TXT")

basename2box <- function(ff) {

  as.integer( str_extract(basename(ff) , "-?\\d+") )

  }



#' @title       List all data directories
#' @param p     path to raw data, default to getOption("path.to.raw")
#' @return      a data.table
#' @export
#' @author      MV
#'
data_dirs <- function(p = getOption("path.to.raw_v2") ) {
  x = list.dirs(p, recursive = FALSE ) %>% sapply(., list.dirs, recursive = FALSE)
  x = data.table(path = unlist(x))
  x[, dir := basename(path)]
  x[, year := dirname(path) %>% basename]
  x
  }




#' @title       Read raw
#' @description Read raw box txt file without any conversion and string splitting
#' @param       filePath  file path to the box file
#' @return      a \code{data.table}
#' @author      MV
#' @export
#' @examples
#' a = readRaw_v2(filePath = "/ds/raw_data_kemp/FIELD/Westerholz/breeding_2016_snb_heaven/07062016/122/BOX0122.TXT")
readRaw_v2 <- function(filePath)  {
    data.table(V = readLines(filePath, skipNul = TRUE) )
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


#' @title         read SNB data
#' @description   read an SNB file as data.table-s (only transponder and Light barrier data are parsed).
#' @param         f path to the snb file
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples
#' x = read_snb_txt_v2(f = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2016/2016.06.07/267/BOX0267.TXT")
#' x = read_snb_txt_v2(f = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2017/2017.04.24/30/BOX0030.TXT")
#'
read_snb_txt_v2 <- function(f) {
  d = readRaw_v2(f = f)
  d = d[ str_detect(V, 'Transponder:|LBO:|LBI:')  ]

  d[, datetime_ := snbstring2date_v2(V) ]
  d[,   sensor_value := str_extract(V, 'Transponder:[ \\t]*([^\\n\\r]*)') ]
  d[is.na(sensor_value), sensor_value := str_extract(V, 'LB[IO]:[ \\t]*([^\\n\\r]*)') %>% toupper ]
  d[, sensor := str_extract(sensor_value, 'LB[IO]|Transponder') %>% tolower]
  d[, sensor_value := str_replace(sensor_value, 'LB[IO]:|Transponder:', '')]
  d[, sensor_value := str_trim(sensor_value)]

  d[, .(datetime_, sensor_value, sensor)]
  }


#' @title         Extract hardware ID.
#' @description   Extract hardware ID from a TXT raw data file.
#' @param         p path to the snb file
#' @return        a  character string (length 1).
#' @author        MV
#' @export
#' @examples
#' hwid(p =  "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2016/2016.06.07/267/BOX0267.TXT")
#'
hwid <- function(p, n = 300) {

  if(length(p) > 1) stop('Something wicked is happening. Stop and check the card!')

  x = readLines(p, n = n)
    if( length(x) > 0) {
      # tt = snbstring2date_v2(x[grep('Initial Time Synchronisation', x)] )[1]

      o = data.table( x[grep('HW-ID:', x)] )

      if(nrow(o) > 0) {
        o[, hwid := str_split(V1, 'HW-ID: ', simplify = TRUE)[2] , by = 1:nrow(o)]
        o = o[!duplicated(hwid), .(hwid)]
        # o[, datetime_ := tt ]
        } else
        o = data.table(hwid = NA, datetime_ = NA )
   } else
   o = data.table(hwid = NA, datetime_ = NA )

  o[, hwid   := as.character(hwid)]
  #o[, datetime_:= as.POSIXct(datetime_)]
  o

  }


#' @title Find if an external software is installed
#' @param nam package name
#' @return logical vector
#' @author MV
#' @export
#' @examples
#' find_installed('secure delete')
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
            paste('<strong>db name:</strong>',     getOption('snbDB2'), '<br>'),
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

install_dektop <- function() {

 system('mkdir -p ~/butler/')

 cmd = paste('cp' , paste0(system.file('os_install', package = 'SNB2'), '/*') , '~/butler/')
 system(cmd)



}