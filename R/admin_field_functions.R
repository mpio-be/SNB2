

#' Plot raw data in the field
#'
#' @param path The path to the BOX001.TXT file. Defaults to the desktop (/home/bt/Desktop/)
#' @param box A box number (defaults to 999)
#'
#' @return An actogram (.pdf) that is located at the specified path and a data table with the underlying data.
#'
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
