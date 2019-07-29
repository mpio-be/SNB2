#EVENTS FUNCTIONS

#' @export
events_v1 = function(x, group_ins_and_outs = TRUE, FUN = "translate_validity_v1", time_threshold = 2, max_distance = 14*60*60, silent = FALSE) {
  x[, datetime_ := as.numeric(datetime_)]
  x = fetch_ins_outs_v1(x, time_threshold = time_threshold); if (nrow(x) == 0)   return()
  
  x = assign_direction_v1(x); if (nrow(x) == 0) return()
  
  if(group_ins_and_outs == FALSE) {
    x[, startt := as.POSIXct(floor(startt), origin = "1970-01-01", tz = "Europe/Berlin")]
    x[, endt := as.POSIXct(floor(endt), origin = "1970-01-01", tz = "Europe/Berlin")]
    x[break_before == 0, tmp1 := "I"]
    x[break_before == 1, tmp1 := "O"]
    x[break_before == 0.5, tmp1 := "?"]
    x[break_after == 0, tmp2 := "I"]
    x[break_after == 1, tmp2 := "O"]
    x[break_after == 0.5, tmp2 := "?"]
    x[, direction_detail := paste0(tmp1, "/", tmp2)]
    x[direction_detail == "?/?", direction := "?"]
    x[direction_detail == "?/I" | direction_detail == "O/?" | direction_detail == "O/I", direction := "IN"]
    x[direction_detail == "I/O" | direction_detail == "I/?" | direction_detail == "?/O", direction := "OUT"]
    x[direction_detail == "I/I", direction := "BACK"]
    x[direction_detail == "O/O", direction := "FRONT"]
    x[, ':=' (break_before = NULL, break_after = NULL, tmp1 = NULL, tmp2 = NULL)]
    
    #remove duplicates
    x = unique(x, by = names(x)[which(names(x) != "id")])
    return(x)
  }
  
  x = concat_events_v1(x, max_distance = max_distance);  if(nrow(x) == 0) return()
  
  x[, in_ := as.POSIXct(floor(in_), origin = "1970-01-01", tz = "Europe/Berlin")]
  x[, out_ := as.POSIXct(floor(out_), origin = "1970-01-01", tz = "Europe/Berlin")]
  
  x = eval(call(FUN,x)); stats = c(c(NA, NA, NA), x[[2]]); class(stats) <- "SNBstats"; x = copy(x[[1]])
  x = unique(x, by = names(x)[which(!(names(x) %in% c("in_r_pk", "out_r_pk", "id")))])
  x = unique(x, by = names(x)[which(names(x) != "id")])
  class(x) <- c("SNBoutput", class(x))
  
  setattr(x, "stats", stats)
  if(silent == FALSE) print(attr(x, "stats"))
  return(x)
}
