# ==========================================================================
# operation on files and directories
# ==========================================================================



#' @title              Check the raw data directory 
#' @description        The function checks the proper directory format: 
#'                     \code{ year/date[prefix]/box/b000.txt}
#' @param       p      path to raw data, default to getOption("path.to.raw")
#' @return             TRUE on success; the offending paths on failure.
#' @author             MV
#' @export
#' @examples
#' \dontrun{
#' well_formated_directory(getOption("path.to.raw_v2"))
#' }
well_formated_directory <- function(p = getOption("path.to.raw_v2") , y = year(Sys.Date()) ) {

  x = data.table( dirs = list.files( paste0(p, y), full.name = TRUE) )

  x[, dirnam := basename(dirs) ]

  isNotDate = x[ ! grepl('^[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}$', dirnam )   ]

  if(nrow(isNotDate) > 0) {
    return(isNotDate)
    stop('Invalid directories')
  } else return(TRUE)

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
#' \dontrun{
#' a = readRaw_v2(filePath = 
#'         "/ds/raw_data_kemp/FIELD/Westerholz/breeding_2016_snb_heaven/07062016/122/BOX0122.TXT")
#' }
readRaw_v2 <- function(filePath)  {
    data.table(V = readLines(filePath, skipNul = TRUE) )
    }



#' @title         read SNB data
#' @description   read an SNB file as data.table-s (only transponder and Light barrier data are parsed).
#' @param         f path to the snb file
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples 
#' x = read_boxtxt(system.file('test_files_SNB', '80', 'BOX0080.TXT', package = 'SNB2'))

read_boxtxt <- function(f) {
  d = readRaw_v2(f = f)
  file_path = str_remove(f, getOption('path.to.raw_v2')) 

  d[, V := str_to_upper(V)]
  d = d[ str_detect(V, 'TRANSPONDER:|LBO:|LBI:')  ]

  d[, datetime_ := snbstring2date_v2(V) ]
  d[,   sensor_value := str_extract(V, 'TRANSPONDER:[ \\t]*([^\\n\\r]*)') ]
  d[is.na(sensor_value), sensor_value := str_extract(V, 'LB[IO]:[ \\t]*([OFF|ON]*)')  ]
  d[, sensor := str_extract(sensor_value, 'LB[IO]|TRANSPONDER') ]
  d[, sensor_value := str_remove(sensor_value, 'LB[IO]:|TRANSPONDER:')]
  d[, sensor_value := str_trim(sensor_value)]
  d[, sensor := str_sub(sensor, 1, 3) %>% str_to_lower]

  # flag garbage 
  d[ str_count(sensor_value) != 16 & sensor == 'tra', g := 1]
  d[ !sensor_value %in% c('ON', 'OFF') & sensor %in% c('lbi', 'lbo'), g := 1]
  #prop garbage (from the total of possibly good lines)
  pg = nrow(d[g == 1])/nrow(d)

  # final subset
  o = d[is.na(g), .(datetime_, sensor_value, sensor)]
  o[, path := file_path ]

  if(nrow(o) == 0) pg = 1

  # set attributes (set file_path too because of empty files)
  setattr(o, 'SNB2', data.frame(box= basename2box(f), path = file_path, garbage = pg) )

  o

  }


#' @title         Extract hardware ID.
#' @description   Extract hardware ID from a TXT raw data file.
#' @param         p path to the snb file
#' @return        a  character string (length 1).
#' @author        MV
#' @export
#' @examples 
#' \dontrun{
#' hwid(p =  "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2016/2016.06.07/267/BOX0267.TXT")
#' }
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