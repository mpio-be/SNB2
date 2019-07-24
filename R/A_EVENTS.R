
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
#'x = SNBevents::fetch_data_v2(con, box = 1, from = "2019-05-01", to = "2019-05-30")
#'x = SNBevents::events_v2(x, cluster_events_threshold = 0.2, group_ins_and_outs = FALSE)
#'x
#'
#'#fetch data for multiple boxes
#' X = list()
#' STATS = list()
#' for(i in c(3, 5, 206, 210)){
#'   x = SNBevents::fetch_data_v2(con, box = i, from = "2019-05-01", to = "2019-05-30")
#'   x = SNBevents::events_v2(x, cluster_events_threshold = 0.2, silent = TRUE)
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
  x = SNBevents::fetch_ins_outs_v2(x, tr_threshold = tr_threshold, broken_LB_threshold = broken_LB_threshold, cluster_events_threshold = cluster_events_threshold); if (nrow(x) == 0)   return()

  x = SNBevents::assign_direction_v2(x); stats = x[[2]];  x = copy(x[[1]]);  if (nrow(x) == 0) return()

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

  x = SNBevents::concat_events_v2(x, max_distance = max_distance);  if(nrow(x) == 0) return()

  x = SNBevents::combine_front_v2(x, cluster_fronts_threshold = cluster_fronts_threshold, no_front = no_front);  if(nrow(x) == 0) return()

  x = eval(call(FUN,x)); stats = c(stats, x[[2]]); class(stats) <- "SNBstats"; x = copy(x[[1]])
  class(x) <- c("SNBoutput", "data.table", "data.frame")
  output = list(x, stats)
  if(stats.out == TRUE)  { if(silent == FALSE) print(output[[2]]); return(output)} else return(x)
}

#' @title Fetch data from databases
#'
#' @description Fetches data from the bxxx databases.
#'
#' @param con A connection as established e.g. by dbcon()
#' @param box The box number for which the data should be fetched.
#' @param from From which point onwards should the data be fetched? Can be a date-time object or an r_pk (column r_pk from the bxxx tables)
#' @param to Until which point should the data be fetched? Can be a date-time object or an r_pk (column r_pk from the bxxx tables)
#'
#' @return A data.table with all the information from the bxxx table, and an additional box column. Any rows where the column datetime_ is NA are removed.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
fetch_data_v2 = function (con, box = NA, from = NA, to = NA) {

  if (is.na(box) | is.na(from) | is.na(to))
    print("Please provide values for \"box\", \"from\" and \"to\"")
  box = sprintf("%03d", as.numeric(box))
  if (is.numeric(from) & is.numeric(to)) {
    x = dbq(con, paste0("SELECT ", box, " as box, UNIX_TIMESTAMP(datetime_) as datetime_, sensor_value, sensor, r_pk, path FROM SNBatWESTERHOLZ_v2.",
                        int2b(box), " WHERE r_pk >= ", from, " AND r_pk <= ",
                        to))

  }
  else {
    x = dbq(con, paste0("SELECT ", box, " as box, UNIX_TIMESTAMP(datetime_) as datetime_, sensor_value, sensor, r_pk, path FROM SNBatWESTERHOLZ_v2.",
                        int2b(box), " WHERE datetime_ >= '", from, "' AND datetime_ <= '",to, "'"))
  }
  x = subset(x, !is.na(datetime_))
  return(x)
}

#' @title Split data into different pieces of activity
#'
#' @description Split data into different pieces of activity, e.g. one "IN" and one "OUT"
#'
#' @param x A data.table as supplied by \link{fetch_data_v2}.
#' @param tr_threshold Time in seconds within which the light barriers are assigned to a specific transponder
#' @param broken_LB_threshold Time in seconds after which a light barrier that is constantly on will be marked as off.
#' @param cluster_events_threshold Time in seconds within which different oieces of activity will be considered as belonging together.
#' @details See \link{events_v2} for further information.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()

fetch_ins_outs_v2 = function (x, tr_threshold = 5, broken_LB_threshold = 600, cluster_events_threshold = 1) {
  #sort data by datetime_
  setkey(x, path, datetime_)
  x = unique(x, by = c("datetime_", "sensor_value", "sensor"))

  #tr_threshold = 5 is hardware-specific, and caused by potential "delays" in the electronics; see also comments below. Do not change after optimization.
  #broken_LB_threshold = 600 should be optimized; Ignores LB information if the light barrier is on for too long.
  x[sensor != "tra", lb_time := (shift(datetime_, 1, last(datetime_), type = 'lead') - datetime_), by = sensor]
  x[lb_time > broken_LB_threshold & sensor_value == "ON", sensor_value := "OFF"]
  x[, lb_time := NULL]

  #define new events by light barriers: in the line after all were turned off a new event starts.
  #ignore cases where a light barrier is turned ON or off twice
  #a transponder belongs to the upcoming event (defined by hardware), unless the time difference to the event after is tr_threshold seconds larger than to the row before; if it is more than cluster_events_threshold away from any LB, don't assign to LB activity.
  x[, lbo := 0]
  x[sensor == "lbo", lbo := ifelse(sensor_value == "ON" & shift(sensor_value[which(sensor == "lbo")], 1, "") != "ON", 1, lbo)]
  x[sensor == "lbo", lbo := ifelse(sensor_value == "OFF" & shift(sensor_value[which(sensor == "lbo")], 1, "") != "OFF", -1, lbo)]

  x[, lbi := 0]
  x[sensor == "lbi", lbi := ifelse(sensor_value == "ON" & shift(sensor_value[which(sensor == "lbi")], 1, "") != "ON", 1, lbi)]
  x[sensor == "lbi", lbi := ifelse(sensor_value == "OFF" & shift(sensor_value[which(sensor == "lbi")], 1, "") != "OFF", -1, lbi)]

  # "event"-definition no. 1: which light barrier is turned on / off?
  x[, event := 0]
  x[lbi != 0, event := lbi]
  x[lbo != 0, event := lbo]


  # "event"-definition no. 2: no. of light barriers on
  x[, event := cumsum(event)]
  x[, event := event - min(event)]

  # a transponder reading belongs to the upcoming activity (defined by hardware), unless it is much closer in time to the previous activity
  # precisely: if the time difference to the activity after is tr_threshold seconds larger than to the activity before, transponder reading is assigned to activity before
  # if transponder reading is more than 2*tr_threshold away from any LB, don't assign to LB activity.
  x[, tra := (datetime_ - shift(datetime_,1,first(datetime_))+tr_threshold) < (shift(datetime_, 1, last(datetime_), type = "lead") - datetime_)]# TRUE: transponder reading belongs to previous activity; FALSE: belongs to following activity

  # "event"-definition no. 3: start or no start of new bird action
  x[, event := ifelse(
    (event == 1 & shift(event, n = 1, fill = 0) == 0 & (shift(sensor, n=1, fill = "") != "tra" | shift(tra, 1, first(tra)) == TRUE)) | #first condition: (one light barrier is "ON" AND in previous row no light barriers "ON" AND (((previous row is not a transponder reading or tra (== whether transponder belongs to this or to the previous; TRUE means previous) in previous row is TRUE)))  )
      (sensor == "tra" & tra == FALSE & event == 0), 1, 0)]
  # also mark new action if the file id changes
  x[path != shift(path, 1, first(path)), event := event+1]

  x[, tra := NULL]

  x[, lbo := NULL]
  x[, lbi := NULL]


  #group events together that are within a time of cluster_events_threshold, disregarding transponder differences => don't set too big!!!
  x[event == 1 & (datetime_ - shift(datetime_, fill = datetime_[1]-cluster_events_threshold-1, type = 'lag')) <= cluster_events_threshold & path == shift(path), event := 0]

  # "event"-definition no. 4: each action receives unique identifier
  # actions are counted based on time differences and light barriers
  x[, event := cumsum(event)]

  #if there are two transponders connected to one event, mark it (see below: event will be duplicated)
    x[sensor == "tra", multiple_tr := length(unique(sensor_value[which(sensor == "tra")])), by = event]
    x[sensor == "tra", transp := sensor_value]
    x[, multiple_tr := na.omit(multiple_tr)[1], by = list(path, event)]#

    x[is.na(multiple_tr) | multiple_tr == 0, multiple_tr := 1]
  setkey(x, path, event, datetime_)
  return(x)
}


#' @title Assign direction to different pieces of activity
#'
#' @description Assign direction to different pieces of activity, e.g. "IN" and "OUT"
#'
#' @param x A data.table as supplied by \link{fetch_ins_outs_v2}.
#' @param hardware_threshold = 0.05 Time in seconds within which a changed of outer and inner light barrier is assumed to be "at the same time". This is defined by the hardware settings and should therefore usually not be changed.
#' @details See \link{events_v2} for further information. Note that after some minor alterations, the output of this function is returned, if in function \link{events_v2} groups_ins_and_outs = FALSE.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
assign_direction_v2 = function(x, hardware_threshold = 0.05) {
  #remove any event that has neither transponder nor both light barriers (so to say: BACK and FRONT without transponder)
  x[, remove := ifelse(length(unique(sensor)) > 1, 0, 1), by = event]
  x = x[remove == 0,]
  x[, remove := NULL]

  #fetch time differences; takes long to calculate
  x[, delta_ON := first(datetime_[which(sensor == 'lbo' & sensor_value == 'ON')]) - first(datetime_[which(sensor == 'lbi' & sensor_value == 'ON')]), by = event]
  x[, delta_OFF := last(datetime_[which(sensor == 'lbo' & sensor_value == 'OFF')]) - last(datetime_[which(sensor == 'lbi' & sensor_value == 'OFF')]), by = event]
  x[,delta_lbo := last(datetime_[which(sensor == 'lbo' & sensor_value == "OFF")]) - first(datetime_[which(sensor == 'lbo' & sensor_value == "ON")]), by = event]
  x[,delta_lbi := last(datetime_[which(sensor == 'lbi' & sensor_value == "OFF")]) - first(datetime_[which(sensor == 'lbi' & sensor_value == "ON")]), by = event]

  x[, startt := min(datetime_), by = event]
  x[, endt := max(datetime_), by = event]
  x[, r_pk := min(r_pk), by = event]

  x[, `:=`(datetime_ = NULL, sensor_value = NULL, sensor = NULL)]
  x[, transp2 := list(list(as.character(na.omit(unique(transp))))), by = event]

  x = unique(x, by = names(x)[-which(names(x) == 'transp2')])
  #remove events that are not yet unique (transponder + NA in transponder column)
  x[, tmp := ifelse(duplicated(x[, event]) |  duplicated(x[, event], fromLast = TRUE), 1, 0)]
  x[tmp == 1, multiple_tr := unique(na.omit(multiple_tr))[1], by = event]
  x[tmp == 1, transp := unique(as.character(na.omit(transp)))[1], by = event] #this adds the transponder and removes the NA
  x[, tmp := NULL]
  x[, transp := NULL]

  x = unique(x, by = names(x)[-which(names(x) == 'transp2')])

  x[, event := NULL]
  x = unique(x, by = names(x)[-which(names(x) == 'transp2')])


  #define direction based on delta light barrier turnon
  # 0=inner before outer; 1=outer before inner; 0.5=no assignment
  # break before - is begin light barrier LBO?
  x[delta_ON <= -hardware_threshold , break_before := 1]
  x[delta_ON >= hardware_threshold , break_before := 0]
  x[delta_ON > -hardware_threshold & delta_ON < hardware_threshold, break_before := 0.5]
  x[is.na(delta_ON) & is.na(delta_lbo), break_before := 0]
  x[is.na(delta_ON) & is.na(delta_lbi), break_before := 1]
  x[is.na(delta_ON) & is.na(delta_lbi) & is.na(delta_lbo), break_before := 0.5]


  #define direction based on delta light barrier turnoff
  # 0=inner after outer; 1=outer after inner; 0.5=no assignment
  # break after - is end light barrier LBO?
  x[delta_OFF < -hardware_threshold , break_after := 0]
  x[delta_OFF > hardware_threshold , break_after := 1]
  x[delta_OFF >= -hardware_threshold & delta_OFF <= hardware_threshold, break_after := 0.5]
  x[is.na(delta_OFF) & is.na(delta_lbo), break_after := 0]
  x[is.na(delta_OFF) & is.na(delta_lbi), break_after := 1]
  x[is.na(delta_OFF) & is.na(delta_lbi) & is.na(delta_lbo), break_after := 0.5]
##
  #if multiple transponders are connected to one event, duplicate this event once for each transponder and set break_before and break_after to 0.5.
  tmp = nrow(x)
  x = x[rep(seq_len(nrow(x)), ifelse(multiple_tr==0,1,multiple_tr)),]
  tmp = nrow(x) - tmp
  x[multiple_tr > 1, break_before := 0.5]
  x[multiple_tr > 1, break_after := 0.5]
  x[multiple_tr == 1, transp := as.character(transp2)]
  x[multiple_tr > 1, transp := as.character(unlist(unique(transp2))), by = r_pk]
  x[transp == "character(0)", transp := NA]
  x[, transp2 := NULL]


  a = c(tmp, max(x[, multiple_tr]), nrow(x[multiple_tr > 1,])/nrow(x))
  x[, ":=" (delta_ON = NULL, delta_OFF = NULL, delta_lbo = NULL, delta_lbi = NULL, multiple_tr = NULL)]
  output = list(x,a)
  return(output)
}

#' @title Groups activity that belongs together.
#'
#' @description Groups activity that belongs together, e.g. IN-OUT, IN-BACK-OUT, ...
#'
#' @param x A data.table as supplied by \link{assign_direction_v2}.
#' @param max_distance Maximum time in seconds within which different pieces of activity will be considered as belonging to a single event.
#' @details See \link{events_v2} for further information.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
concat_events_v2 = function(x, max_distance = 14*60*60) {
  # aim:
  # combine actions that belong together as one behavioural process
  # examples:
  # outside -> inside (=IN) and corresponding inside -> outside (=OUT) for one IN-OUT
  # outside -> inside (=IN) and corresponding inside->inside (=BACK) for one IN-BACK
  # separate actions that do not belong together: one behavioural process (e.g. IN-OUT) starts outside the nestbox
  # "event"-definition no. 5: identifier for each action starting outside (e.g. IN) and its corresponding end outside (e.g. OUT); identifiers are not unique between individuals (transponders)
  # new identifier is also assigned, if bird might have been outside box
  # new identifier:
  # - focal line: birds starts from outside
  # - previous line: birds ends at outside
  # - focal line: bird start unknown AND previous line: bird end unknown
  # same identifier:
  # - focal line: bird starts from inside AND previous line: bird ends at inside
  # - focal line: bird starts from inside AND previous line: bird end unknown
  # - focal line: bird start unknown AND previous line: bird ends at inside

  #combine events that belong together to new events
  x[is.na(transp), transp := '']
  x[, event := as.numeric(cumsum(ifelse(
        (break_before+shift(break_after,1,0.5) >= 1) | (startt - shift(startt, 1, startt[1])) > max_distance,  1, 0))), by = 'transp']

  #classify side bird started/finished action
  # dir1: based on order, in which light barriers turned on, i.e. initial light barrier
  x[break_before == 0, dir1 := "I"]   # initial LB is LBI -> bird starts from inside = I
  x[break_before == 0.5, dir1 := "?"]# not clear which turned on first -> ?
  x[break_before == 1, dir1 := "O"]# initial LB is LBO -> bird starts from outside = O
  # dir2: based on order, in which light barriers turned off, i.e. final light barrier
  x[break_after == 0, dir2 := "I"]# final LB is LBI -> bird ends at inside = I
  x[break_after == 0.5, dir2 := "?"]# not clear which turned off first -> ?
  x[break_after == 1, dir2 := "O"]# final LB is LBO -> bird ends at outside = O

  #if the next/previous action's side is clear, mark accordingly with lower-case letters
  x[, dir1 := ifelse(dir1 == "?" & shift(dir2, 1, '') == "I", "i", dir1), by = transp]# i= previous inside
  x[, dir1 := ifelse(dir1 == "?" & shift(dir2, 1, '') == "O", "o", dir1), by = transp]# o=previous outside
  x[, dir2 := ifelse(dir2 == "?" & shift(dir1, 1, '', type = 'lead') == "I", "i", dir2), by = transp]# i=next is inside
  x[, dir2 := ifelse(dir2 == "?" & shift(dir1, 1, '', type = 'lead') == "O", "o", dir2), by = transp]# o=next is outside



  x[, `:=`(in_ = first(startt),
           out_ = last(startt),
           in_duration = round((first(endt) - first(startt)), digits = 3),
           out_duration = round((last(endt) - last(startt)), digits = 3),
           in_r_pk = first(r_pk),
           out_r_pk = last(r_pk),
           val1 = first(dir1),# side bird starts for IN
           val2 = first(dir2),# side bird ends for IN
           val3 = ifelse(length(dir1) > 1, last(dir1), as.character(NA)),# side bird starts for OUT
           val4 = ifelse(length(dir2) > 1, last(dir2), as.character(NA))# side bird ends for OUT
           ),
    by = list(transp, event)]

  # if the actions assigned to each other are from the same data-row, set all information for final action to unknown
  x[in_r_pk == out_r_pk, ':=' (out_ = NA, out_duration = NA, out_r_pk = NA, val3 = NA, val4 = NA)]

  # paste information on initial and final side for first action (e.g. IN) and final action (e.g. OUT), respectively
  # if first and last action identical, only use information from first action
  x[, validity := ifelse(!is.na(val3),
                         paste(paste(val1, val2, sep = '/'), paste(val3, val4, sep = '/'), sep = '|'),
                         paste(val1, val2, sep = '/'))]

  x[, ':=' (r_pk = NULL, startt = NULL, endt = NULL, event = NULL, break_before = NULL, break_after = NULL, dir1 = NULL, dir2 = NULL, val1 = NULL, val2 = NULL, val3 = NULL, val4 = NULL)]

  x = unique(x, by = c(names(x)))
  x[transp == '', transp := NA]
  setkey(x, box, in_, out_, transp)
  return(x)


}


#' @title Groups FRONTs that are close in time.
#'
#' @description Example: A bird pokes its head into the nest opening and pulls it back, this can be scored as different events or as belonging to the same one.
#'
#' @param x A data.table as supplied by \link{concat_direction_v2}.
#' @param cluster_fronts_threshold Time in seconds within which FRONTs are grouped together.
#' @param no_front Define which activities are assumed to NOT be a front. Defaults to anything that is completely unclear, or that ended or started inside of the box (e.g. OUT, IN, BACK, ...)
#' @details See \link{events_v2} for further information.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
combine_front_v2 = function(x, cluster_fronts_threshold = 5, no_front = NULL){
  # aim: assign transponders to FRONTS without transponder number
  # change NA transp to ''
  if(is.null(no_front)) { tmp = toupper(unique(x[,validity])); no_front = c(tmp[grep("I", tmp)], "?/?|?/?") }

  #NA transp to ''
  x[is.na(transp), transp := '']

  # new column to mark behaviours that need to be separated/combined:
  # mark (for separation), if time difference is larger than threshold
  # this is a safe-guard to avoid sorting FRONTS to wrong transponders in vicinity
  x[, combine := ifelse((in_ - shift(ifelse(!is.na(out_), out_, in_), 1, first(in_)) - shift(ifelse(!is.na(out_duration), out_duration, in_duration),1,0)) > cluster_fronts_threshold, 1, 0)]

  # mark (for separation), if transp changes (to no value or other value)
  # no transponder can be assigned to FRONT
  x[!is.na(transp), combine := ifelse(transp != shift(transp, 1, first(transp)), 1, combine) ]
  x[is.na(combine), combine := 1]
  # mark (for combination), if first started and ended outside in both focal and previous line (indicates front events that belong together)
  x[toupper(validity) %in% no_front | shift(toupper(validity), 1, '') %in% no_front, combine := 1]
  # separate based on the new column
  x[, combine := cumsum(combine)]

  # reduce side information to relevant information
  x[, ':=' (in_ = first(in_), in_duration = first(in_duration), in_r_pk = first(in_r_pk),
            out_ = last(out_), out_duration = last(out_duration), out_r_pk = last(out_r_pk),
            validity = ifelse(nchar(last(validity)) > 3,
                              paste(substring(first(validity), 1, 3), substring(last(validity), 5, 7), sep = '|'),
                              first(validity))), by = combine]

  x[, combine := NULL]
  x = unique(x, by = names(x))

  x[, in_ := as.POSIXct(floor(in_), origin = "1970-01-01", tz = "Europe/Berlin")]
  x[, out_ := as.POSIXct(floor(out_), origin = "1970-01-01", tz = "Europe/Berlin")]
  x[transp == '', transp := NA]

}


#' @title Simplify direction of activity
#'
#' @description Simplify the information of the direction: Translate the detailed directional information into something radable.
#'
#' @param x A data.table as supplied by \link{combine_front_v2}.
#' @details See \link{events_v2} for further information.
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
translate_validity_v2 = function(x) {
  x[, direction_detail := validity]
  #make everything IN-OUT that isn't FRONT
  x[, direction := 'IN-OUT']
  #everything with 2 pieces of information is FRONT unless...
  x[nchar(direction_detail) == 3, direction := "FRONT"]
  #unless it is I/O (=OUT) or O/I (=IN)
  x[toupper(direction_detail) == "I/O", direction := "OUT"]
  x[toupper(direction_detail) == "O/I", direction := "IN"]
  #everything with Os only is IN #<- based on video material, 90% of the time
  x[toupper(direction_detail) == "O/O|O/O", direction := "IN"]
  x[, validity := NULL]
  CORRECT = c('O/O', 'O/I|I/O')
  a = c(nrow(x[toupper(direction_detail) %in% CORRECT,])/nrow(x),
      nrow(x[toupper(direction_detail) %in% CORRECT & !is.na(transp),])/nrow(x[!is.na(transp),]))
  output = list(x, a)
  return(output)
}
