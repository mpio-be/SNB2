#' @title         read SF file
#' @description   read one SF file as data.table
#' @param         f path to the SF file
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples
#' x = read_sf_txt(f = system.file(package = 'SNB2', 'sample_files', 'BOX1020.TXT'))

read_sf_txt <- function(f) {
  d = readRaw_v2(f)
  d[, transp := str_extract(V,  "Transponder:[ \\t](\\w{16}\\b)")]
  d = d[!is.na(transp)]
  d[, transp := str_replace(transp, 'Transponder: ', '')]
  d[, datetime_ := snbstring2date_v2(V) ]
  d[, .(datetime_, transp)]

 }


#' @title         read SF directory
#' @description   read all SF files in a given root directory
#' @param         D path to directory
#' @param         nextid next file ID in the db table.
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples
#' read_sf_dir('/ds/raw_data_kemp/FIELD/Westerholz/SF/RAWDATA/2018/')

read_sf_dir <- function(D, nextid = NA, basepath = getOption('path.to.raw_v2_SF')) {

 x = data.table( path = list.files(D, all.files = TRUE, full.names = TRUE, recursive = TRUE, pattern = 'TXT$') )
 x[, feeder := path2box(path)]
 x[, id := if (is.na(nextid)) NA else nextid : .N ]
 
 d = x[,read_sf_txt(path), by = path]

 d = merge(d, x, by = 'path', sort = FALSE)

 d[, path := str_replace(path, basepath, '/')]

 d[, pk := NA]

 d[, .(feeder, datetime_, transp, path, id, pk)]


 }





