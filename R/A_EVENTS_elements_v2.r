#' @title Fetch data from databases
#'
#' @description Fetches data from the bxxx databases.
#'
#' @param con A connection as established e.g. by dbcon()
#' @param box The box number for which the data should be fetched.
#' @param from From which point onwards should the data be fetched? Can be a date-time object or an r_pk (column r_pk from the bxxx tables)
#' @param to Until which point should the data be fetched? Can be a date-time object or an r_pk (column r_pk from the bxxx tables)
#' @return A data.table with all the information from the bxxx table, and an additional box column. Any rows where the column datetime_ is NA are removed.
#' @export
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()
fetch_data_v2 = function (con, box = NA, from = NA, to = NA) {
 .Defunct()
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
#' @export
#' @examples
#' #Not run
#' #see help of function events_v2
#' ?events_v2()

fetch_ins_outs_v2 = function (x, tr_threshold = 5, broken_LB_threshold = 600, cluster_events_threshold = 1) {
  x = subset(x, !is.na(datetime_))
  
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
#' @export
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
#' @export
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
  #x[in_r_pk == out_r_pk, ':=' (out_ = NA, out_duration = NA, out_r_pk = NA, val3 = NA, val4 = NA)]

  # paste information on initial and final side for first action (e.g. IN) and final action (e.g. OUT), respectively
  # if first and last action identical, only use information from first action
  x[, validity := ifelse(!is.na(val3),
                         paste(paste(val1, val2, sep = '/'), paste(val3, val4, sep = '/'), sep = '|'),
                         paste(val1, val2, sep = '/'))]

  x[, ':=' (r_pk = NULL, startt = NULL, endt = NULL, event = NULL, break_before = NULL, break_after = NULL, dir1 = NULL, dir2 = NULL, val1 = NULL, val2 = NULL, val3 = NULL, val4 = NULL)]

  x = unique(x, by = c(names(x)))
  x[transp == '', transp := NA]
  setkey(x, in_, out_, transp)
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
#' @export
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

  x[transp == '', transp := NA]

}


#' @title Simplify direction of activity
#'
#' @description Simplify the information of the direction: Translate the detailed directional information into something radable.
#'
#' @param x A data.table as supplied by \link{combine_front_v2}.
#' @details See \link{events_v2} for further information.
#' @export
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


#methods definitions
#'@export
setClass("SNBstats",
         slots = c(x="vector"))
#'@export
setMethod("print", signature(x="SNBstats"), function(x) {
  print(paste0("No. duplicated rows: ", x[1]))
  print(paste0("Max no. transponders/line: ", x[2]))
  print(paste0("Mean no. transponders/line: ", x[3]))
  print(paste0("Proportion of reliable lines: ", x[4]))
  print(paste0("Proportion of reliable lines with transponder: ", x[5]))

})

#' @export
setClass("SNBoutput",
         slots = c(x="data.table"))
#' @export
setMethod("plot", signature(x="SNBoutput"), function(x, ylim = c(min(X[,date_in]), max(X[,date_out]))) {
  copy(x) -> X
  X[, count := length(out_), by = transp]
  X[is.na(out_), out_ := as.POSIXct(in_)]
  X[is.na(out_duration), out_duration := in_duration]
  X[, date_in := as.IDate(in_)]
  X[, time_in := as.numeric(as.ITime(in_)/60/60)]
  X[, date_out := as.IDate(out_)]
  X[, time_out := as.numeric((as.ITime(out_)+out_duration)/60/60)]
  X[is.na(transp), transp := '']
  X[, transp := factor(transp, levels = names(sort(table(transp), decreasing = TRUE)))]
  X[, COL := as.numeric(transp)]
  X[, offset := as.numeric(transp)]; X[, offset := offset/max(offset)]; X[, offset := offset - mean(offset)]
  sleep = X[date_in != date_out, ]
  if(nrow(sleep) > 0) {
  sleep1 = copy(sleep); sleep1[, ':=' (date_out = date_in, time_out = 24)]
  sleep2 = copy(sleep); sleep2[, ':=' (date_in = date_out, time_in = 0)]
  X = rbindlist(list(X[date_in == date_out, ], sleep1, sleep2))
  }

  #YLIM = c(min(X[,date_in]), max(X[,date_out]))
  par(las = 1, mar = c(5.1, 7.1, 0.1, 0.1))
  plot(1:2,1:2, type = 'n', xlim = c(0, 24), xlab = "Time of day", ylab = '', yaxt = 'n', ylim = ylim)
  axis(2, at = 13000:30000, labels = FALSE, tcl = -0.2)
  axis(2, at =  axTicks(2), labels = as.IDate(axTicks(2)))
  arrows(X[,time_in], X[,date_in]+X[,offset], X[,time_out], X[,date_out]+X[,offset], length = 0, col = X[,COL])
  points(X[,time_in], X[,date_in]+X[,offset], col = X[,COL], pch = '|', cex = 0.3)

  X = unique(subset(X, select = c(transp, COL, count)))
  X[, COL2 := rep(palette(), 10)[COL]]
  return(X)
})
