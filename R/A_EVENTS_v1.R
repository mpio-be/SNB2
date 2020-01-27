#' @title Fetch events data for SNB version _v1
#' @description This function fetches individual in and out events based on a time threshold and transponder readings.
#' If two transponder readings occur close in time, the rows in between are removed, because it is unclear to which
#' individual this data belongs, and the information about the direction of the movement is marked as "unclear". Directions are assigned based on the light barriers and the data is grouped together accordingly.
#'
#' @param x a data.table, the "raw" data, as provided for instance by the function dbq()
#' @param group_ins_and_outs logical. Should INs and OUTs be grouped together? Defaults to TRUE.
#' @param FUN the name of a function. How should detailed directions based on light barriers be transferred into readable directions? Defaults to the function \link{translate_validity_v1}. 
#' @param time_threshold Time in seconds. If the time difference between two consecutive lines is larger or equal to time_threshold, the rows will be treated as independent pieces of activity. Defaults to 2.
#' @param max_distance Maximum time in secondswithin which INs and OUTs will be grouped together. Example: Set relatively small when considering provisioning data, large when looking at overnight sleep. 
#' @param silent logical. Should the stats about the data quality be printed? Defaults to TRUE.
#'
#' @return a data.table where individual ins and outs are grouped together that is homologous to the one returned by the function events_v2. For details about the output see \link{events_v2}. Note that the column "path" that links to the raw data is here called "id" for reasons of backwards-compatibility.
#' @details Each subfunction performs a specific task. The tasks can be described as follows:\enumerate{
#' \item{\link{fetch_ins_outs_v1}}: Separate different pieces of activity and assign the corresponding transponder. Consecutive rows are assigned to different pieces of activity, if the time difference between consecutive lines is smaller than time_threshold seconds, and if the transponder number changes.
#' \item{\link{assign_direction_v1}}: Remove any event that has only activity at one light barrier and no transponder connected to it (for example FRONT or BACK without transponder). Then, fetch time difference between conscutive pieces of activity and assign a transponder to them, if possible. Assign which light barrier is triggered first (=position of the bird at the beginning of the activity: o means "outside", i means "inside"; "O/I" therefore means that the event started on the outside and ended on the inside and therefore is an "IN") and which light barrier is triggered last (=position of the bird at the end of the activity). If more than one transponder is connected to one event, split the event, remove any lines between the different transponders, and set the respective beginning and end directions to "unclear".
#' \item{\link{concat_events_v1}}: Combine activities that belong together based on the transponder reading, and on the light barrier information using the following rulse:\itemize{
#'     \item If activity starts with the bird being outside, never combine with the previous event.
#'     \item If activity ends with the bird being outside, never combine with the previous event.
#'     \item If the start position of the bird is unclear, only combine if the previous activity ended with an "inside". Mark the start position with a lower-case.
#'     \item If the end position of the bird is unclear, only combine if the next activity starts with an "inside". Mark the start position with a lower-case. Remove redundant information.}
#' \item{\link{translate_validity_v1}}: Function that assigns easy-to-interpret directions (e.g. "IN-OUT") to the detailed but confusing directions given by the light barriers (e.g. "o/I|I/O"). The default function uses the following rules (in the specified order):\enumerate{
#'     \item{assign IN-OUT to all lines (note that some of this will be over-written by the next rules!)}
#'     \item{make everything where only a single activity is recorded (e.g. concat_events_v2 did not combine 2 or more events!) as FRONT unless...}
#'     \item{unless it is I/O (=OUT) or O/I (=IN)}
#'  }
#'}

#' @note There is a wrapper function which can be used to fetch data across the two databases and across boxes: \link{eva}
#' @author LS
#' @seealso \link{eva}
#' @export
#' @examples 
#' #Note that although the raw data differ stubstantially, the functions events_v1 and events_v2 can be used similarly.
#' #The help is therefore kept short here, see ?events_v2 for further examples.
#' con = dbcon('YOUR_USERNAME')
#' x_raw = dbq(con, paste0("SELECT * FROM SNBatWESTERHOLZ.b033 WHERE datetime_ >= '2016-05-01 00:00:00' AND datetime_ <= '2016-05-31 12:00:00'"))
#' x_raw[, box := 33]
#' x = events_v1(x_raw, group_ins_and_outs = TRUE, FUN = "translate_validity_v1", time_threshold = 2, max_distance = 16*60*60, silent = FALSE)
#' plot(x)
#' 
#' #fetch the individual pieces of activity
#' x = events_v1(x_raw, group_ins_and_outs = FALSE, FUN = "translate_validity_v1", time_threshold = 2, max_distance = 16*60*60, silent = FALSE)
#' 

events_v1 = function(x, setTZ = "Etc/GMT-2", group_ins_and_outs = TRUE, FUN = "translate_validity_v1", time_threshold = 2, max_distance = 16*60*60, silent = FALSE) {
  x[, datetime_ := as.numeric(fastPOSIXct(datetime_, tz = "GMT"))]
  x = fetch_ins_outs_v1(x, time_threshold = time_threshold); if (nrow(x) == 0)   return()
  
  x = assign_direction_v1(x); if (nrow(x) == 0) return()
  
  if(group_ins_and_outs == FALSE) {
    x[, startt := as.POSIXct(floor(startt), origin = "1970-01-01", tz = "GMT")]
    x[, endt := as.POSIXct(floor(endt), origin = "1970-01-01", tz = "GMT")]
    x[, startt := force.tz(startt, new.tz = setTZ)]
    x[, endt := force.tz(endt, new.tz = setTZ)]
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
  
  x = eval(call(FUN,x)); stats = c(c(NA, NA, NA), x[[2]]); class(stats) <- "SNBstats"; x = copy(x[[1]])
  x = unique(x, by = names(x)[which(!(names(x) %in% c("in_r_pk", "out_r_pk", "id")))])
  x = unique(x, by = names(x)[which(names(x) != "id")])
  
  x[, in_ := as.POSIXct(floor(in_), origin = "1970-01-01", tz = "GMT")]
  x[, out_ := as.POSIXct(floor(out_), origin = "1970-01-01", tz = "GMT")]
  x[, in_ := force.tz(in_, new.tz = setTZ)]
  x[, out_ := force.tz(out_, new.tz = setTZ)]
  
  class(x) <- c("SNBoutput", class(x))
  setattr(x, "stats", stats)
  if(silent == FALSE)  {print(paste0("Timezone: ", setTZ)); print(attr(x, "stats"))  }
  return(x)
}
