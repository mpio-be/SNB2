
# NOTES:
  # hw_id = hardware_ID (box id)
  # bat volt is checked twice a day by toe box (use for box continuity)
  # time sync happens at 2 in the night (use for box continuity)


#' @title       Read raw
#' @description Read raw box txt file without any conversion and string splitting
#' @param       filePath  file path to the box file
#' @return      a \code{data.table}
#' @author      MV
#' @examples
#' a = readRaw_v2(filePath = "/ds/raw_data_kemp/FIELD/Westerholz/breeding_2016_snb_heaven/07062016/122/BOX0122.TXT")
readRaw_v2 <- function(filePath)  {
    data.table(V = readLines(filePath, skipNul = TRUE) )
    }

#' @title 		  Extract date
#' @description	Extract date from raw data string
#' @param 		  x character vector(s)
#' @return 		  \code{POSIXct} object
#' @author  	  MV
#' @examples	  snbstring2date_v2(x = '20170418-171742.202 Transponder: 4B76C4B43A6F0001')
#' snbstring2date_v2(x = '20170418-171742 Transponder: 4B76C4B43A6F0001')
snbstring2date_v2 <- function(x) {

	o = str_extract(x, '(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})\\.(\\d{3})') # ms
	if(is.na(o))
    o = str_extract(x, '(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})') # s

  strptime(o, "%Y%m%d-%H%M%OS") %>% as.POSIXct

	}


#' @title 			  read SNB data
#' @description		read an SNB file as data.table-s (only transponder and Light barrier data are parsed).
#' @param         filePath path to the snb file
#' @return 			  a  \code{data.table} .
#' @author  		  MV
#' @examples
#' x = read_snb_txt_v2(filePath = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2016/2016.06.07/267/BOX0267.TXT")
#' x = read_snb_txt_v2(filePath = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2017/2017.04.24/30/BOX0030.TXT")
#'
read_snb_txt_v2 <- function(filePath) {
  d = readRaw_v2(filePath = filePath)
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