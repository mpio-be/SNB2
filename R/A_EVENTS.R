
#' @title Pipeline function to extract events
#'
#' @description Calls a set of sub-functions to extract information about box entries and exits. Note that all activity that is not connected with any transponder and triggers only one light barrier is removed. The pros and cons of different parameter choices can be assessed via \link{SNBvsVid_v2}, which performs a comparison with existing video data during provisioning. Note: DATA IS MADE UNIQUE FOR NOW (excepting r_pk or course)!
#'
#' @param x A data.table with raw data as provided by the function \link{fetch_data_v2}
#' @param group_ins_and_outs logical. Should every piece of activity be returned separately (e.g. to merge with observational data), or should they be grouped together to "INs" and "OUTs"? Defaults to TRUE.
#' @param FUN function to translate detailed information about the direction of the activity. Defaults to the function \link{translate_validity_v2}, which assigns "IN-OUT" and "FRONT"
#' @param stats.out logical. Should statistics about the data quality be returned? Defaults to TRUE.

#' @param tr_threshold Optional argument passed to function \link{fetch_ins_outs_v2}. Time in seconds where the light barriers are assigned to a specific transponder. Defaults to 5.
#' @param broken_LB_threshold Optional argument passed to function \link{fetch_ins_outs_v2}. Time in seconds after which a light barrier that is constantly on will be marked as "OFF". Defaults to 600. Experimental.
#' @param cluster_events_threshold Optional argument passed to function \link{fetch_ins_outs_v2}. Time in seconds within which different pieces of activity will be considered as belonging to a single event. Defaults to 1. Note this parameter may reduce the computing time at the cost of detail.
#' @param max_distance Optional argument passed to function \link{concat_events_v2}. Maximum time in seconds within which two pieces of activity may be grouped together. Defaults to 14 hours.
#' @param cluster_fronts_threshold Optional argument passed to function \link{combine_fronts_v2}. Time in seconds within which "FRONT" events of the same transponder will be grouped together.
#' @param no_front NULL or character(). Optional argument passed to function \link{combine_fronts_v2}. Defines which directions are assumed to be a "FRONT" when events are grouped by the function \link{combine_front_v2}. Defaults to NULL, which means that everything where no pass through the nestbox opening was registered is scored as a "FRONT".
#' @return \itemize{\item{If group_ins_and_outs = TRUE and stats.out = FALSE:}
#' {data.table of SNB events with 10 columns. Use plot(this_datatable) to immediately plot this data.} \itemize{
#'   \item{box: } box number
#'   \item{transp: } transponder connected to the event
#'   \item{in_: } datetime of the start of the activity
#'   \item{out_: } datetime of the end of the activity
#'   \item{in_duration: } how long did the activity in column "in_" take (e.g. the bird might have spent 5 seconds in the nest opening before entering properly)
#'   \item{out_duration: } how long did the activity in the out_ column take?
#'   \item{in_r_pk: } link to the raw bxxx tables: r_pk of in_
#'   \item{out_r_pk: } link to the raw bxxx tables: r_pk of out_
#'   \item{direction_detail: } Full information about the direction of movements as specified by the light barriers. Formatted e.g. as O/I|I/O, containing the information (position of bird at the beginning of the "in_")/(position of the bird at the end of the "in_")|(position of bird at the beginning of the "out_")/(position of the bird at the end of the "out_"). Capital letters mean that this is true light barrier information, lower-case letters mean that the position of the bird was inferred by the next or previous activity (e.g. and OUT is usually followed by an IN).
#'   \item{direction: } Summarized directional information as specified by the function \link{translate_validity_v2}, e.g. "IN" or "OUT". Note that this function can be supplied as an argument to the function, so that it is easy to adjust to any needs.
#'   }}
#'  \item{If group_ins_and_outs = TRUE and stats.out = TRUE:}{ List of two, where the first list element is the data table as described above, and the second list element contains information about how reliable the specific data probably is. Use print(these_stats) to view details.}
#'  \item{If group_ins_and_outs = FALSE: }{A data.table with the following information:}{\itemize{
#'  \item{box: } The box number
#'  \item{r_pk: } The r_pk referring to the original bxxx table.
#'  \item{path: } The path the the raw data file
#'  \item{startt: } The start of the activity (rounded to the nearest second)
#'  \item{endt: } The end of the activity (rounded to the nearest second)
#'  \item{transp: } The transponder
#'  \item{direction_detail: } Detailed directional information, where the first letter indicates where the event started (O = outside, I = inside, ? = unclear), and the second letter where is ended. "I/O" would for instance mean "movement from the inside to the outside" -> a bord left the nest box.
#'  \item{direction: } Easy-to-interpret directional information. Note that this assignment is a rough estimate.
#'  }
#'}
#' @details Each subfunction performs a specific task. The tasks can be described as follows:\enumerate{
#' \item{\link{fetch_ins_outs_v2}}: Separate different pieces of activity and assign the corresponding transponder. Consecutive rows are assigned to different pieces of activity, if both light barriers are off for a certain time. This time cutoff is defined by the variable 'cluster_events_threshold'. Note that the data is split only by the light barrier information, if cluster_events_threshold=0. Transponders are then assigned to any event within "tr_threshold" seconds of a transponder reading. If a light barrier is constantly ON (e.g. because of dirt), it is set to OFF after broken_LB_threshold seconds (experimental!).
#' \item{\link{assign_direction_v2}}: Remove any event that has only activity at one light barrier and no transponder connected to it (for example FRONT or BACK without transponder). Then, fetch time difference between conscutive pieces of activity and assign a transponder to them, if possible. Assign which light barrier is triggered first (=position of the bird at the beginning of the activity: o means "outside", i means "inside"; "O/I" therefore means that the event started on the outside and ended on the inside and therefore is an "IN") and which light barrier is triggered last (=position of the bird at the end of the activity) (Note that the light barriers have a pulse every 50 milliseconds; if both lightbarriers are triggered within 50 ms, their order cannot be distinguished, and the hardware_threshold should therefore be kept at 50 ms= 0.05s. In this case the position of the bird is unclear = "?") If more than one transponder is connected to one event, duplicate the event and set the beginning and end directions to "unclear".
#' \item{\link{concat_events_v2}}: Combine activities that belong together based on the transponder reading, and on the light barrier information using the following rulse:\itemize{
#'     \item If activity starts with the bird being outside, never combine with the previous event.
#'     \item If activity ends with the bird being outside, never combine with the previous event.
#'     \item If the start position of the bird is unclear, only combine if the previous activity ended with an "inside". Mark the start position with a lower-case.
#'     \item If the end position of the bird is unclear, only combine if the next activity starts with an "inside". Mark the start position with a lower-case. Remove redundant information.}
#' \item{\link{combine_front_v2}}: Assign transponder to FRONTs without transponder if they are within cluster_fronts_threshold seconds of a transponder reading. This is implemented because FRONT events  preceding an IN of a transpondered bird are often very short and often the transponder is not noted although when looking at the data the identity of the bird is easy to see.
#' \item{\link{translate_validity_v2}}: Function that assigns easy-to-interpret directions (e.g. "IN-OUT") to the detailed but confusing directions given by the light barriers (e.g. "o/I|I/O"). The default function uses the following rules (in the specified order):\enumerate{
#'     \item{assign IN-OUT to all lines (note that some of this will be over-written by the next rules!)}
#'     \item{make everything where only a single activity is recorded (e.g. concat_events_v2 did not combine 2 or more events!) as FRONT unless...}
#'     \item{unless it is I/O (=OUT) or O/I (=IN) (based on the video data) or ...}
#'     \item{has Os only (=IN) <- based on video material, 90% of the time}
#'  }
#'}
#'
#' }
#' @author LS
#' @examples
#'##Not run
#'#establish connection
#'con = dbcon('YOUR_USER_NAME')
#'
#'#fetch data for a single box
#'x = fetch_data_v2(con, box = 1, from = "2019-05-01", to = "2019-05-30")
#'x = events_v2(x, cluster_events_threshold = 0.2)
#'plot(x[[1]])
#'
#'#fetch data where INs and OUTs are not yet combined
#'x = fetch_data_v2(con, box = 1, from = "2019-05-01", to = "2019-05-30")
#'x = events_v2(x, cluster_events_threshold = 0.2, group_ins_and_outs = FALSE)
#'x
#'
#'#fetch data for multiple boxes
#' X = list()
#' STATS = list()
#' for(i in c(3, 5, 206, 210)){
#'   x = fetch_data_v2(con, box = i, from = "2019-05-01", to = "2019-05-30")
#'   x = events_v2(x, cluster_events_threshold = 0.2, silent = TRUE)
#'   X[[length(X)+1]] <- x[[1]]
#'   STATS[[length(STATS)+1]] <- x[[2]]
#' }
#' stats = as.data.table(do.call(rbind, STATS))
#'
#' setnames(stats, names(stats), c("dupl", "max_tr", "mean_tr", "prop_reliable", "prop_reliable_tr"))
#' #Plot method for SNB data (note that data has class "SNBoutput")
#' plot(X[[1]])
#' plot(X[[3]])
#' #note that the histogram is most informative when many boxes are run
#' par(mfrow = c(2,1))
#' hist(stats$prop_reliable, 20, xlab = "Proportion of high quality data\nall data", main = '')
#' hist(stats$prop_reliable_tr, 20, xlab = "Proportion of high quality data\ntransponder data", main = '')
#' summary(stats$prop_reliable); summary(stats$prop_reliable_tr)

events_v2 = function(x, group_ins_and_outs = TRUE, FUN = "translate_validity_v2", stats.out = TRUE, tr_threshold = 5, broken_LB_threshold = 600, cluster_events_threshold = 2, max_distance = 14*60*60, cluster_fronts_threshold = 5, no_front = NULL, silent = FALSE)
{
  x = fetch_ins_outs_v2(x, tr_threshold = tr_threshold, broken_LB_threshold = broken_LB_threshold, cluster_events_threshold = cluster_events_threshold); if (nrow(x) == 0)   return()

  x = assign_direction_v2(x); stats = x[[2]];  x = copy(x[[1]]);  if (nrow(x) == 0) return()

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
      return(x)
  }

  x = concat_events_v2(x, max_distance = max_distance);  if(nrow(x) == 0) return()

  x = combine_front_v2(x, cluster_fronts_threshold = cluster_fronts_threshold, no_front = no_front);  if(nrow(x) == 0) return()

  x = eval(call(FUN,x)); stats = c(stats, x[[2]]); class(stats) <- "SNBstats"; x = copy(x[[1]])
  class(x) <- c("SNBoutput", "data.table", "data.frame")
  output = list(x, stats)
  if(stats.out == TRUE)  { if(silent == FALSE) print(output[[2]]); return(output)} else return(x)
}
