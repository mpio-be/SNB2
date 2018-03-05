#' @title         read SF data
#' @description   read an SF file as data.table
#' @param         f path to the SF file
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples
#' x = read_sf_txt_v2(f = system.file(package = 'SNB2', 'sample_files', 'BOX1020.TXT'))


read_sf_txt_v2 <- function(f) {
  d = readRaw_v2(f)
  d[, transp := str_extract(V,  "Transponder:[ \\t](\\w{16}\\b)")]
  d = d[!is.na(transp)]
  d[, transp := str_replace(transp, 'Transponder: ', '')]
  d[, datetime_ := snbstring2date_v2(V) ]
  d[, .(datetime_, transp)]

 }