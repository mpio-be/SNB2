
#' @export
fetch_data_v2 = function (con, box = NA, from = NA, to = NA) {

  if (is.na(box) | is.na(from) | is.na(to)) 
  print("Please provide values for \"box\", \"from\" and \"to\"")
  #dbq(con, paste("USE", getOption("snbDB")))
  dbq(con, "USE SNBatWESTERHOLZ_v2")
  box = sprintf("%03d", as.numeric(box))
  if (is.numeric(from) & is.numeric(to)) {
  x = dbq(con, paste0("SELECT ", box, " as box, UNIX_TIMESTAMP(datetime_) as datetime_, sensor_value, sensor, r_pk, id FROM ", 
                      paste0("b", box), " WHERE r_pk >= ", from, " AND r_pk <= ", 
                      to))

  }
  else {
  x = dbq(con, paste0("SELECT ", box, " as box, UNIX_TIMESTAMP(datetime_) as datetime_, sensor_value, sensor, r_pk, id FROM ", 
                      paste0("b", box), " WHERE datetime_ >= ", from, " AND datetime_ <= ", 
                      to))
  }
  x = subset(x, !is.na(datetime_))
  return(x)
  }


#' @export
fetch_ins_outs_v2 = function (x, tr_threshold = 5, safety_threshold = 600) {
  #if one light barrier is on for more than safety_threshold seconds, mark new event after at time difference larger than safety_threshold seconds; this is a cutoff designed to make sure that a dirty light barrier does not lead to a loss of events.
  x[sensor != "tra", lb_time := shift(datetime_, 1, last(datetime_), type = 'lead') - datetime_, by = sensor]
  x[lb_time > safety_threshold & sensor_value == "ON", sensor_value := "OFF"]
  x[, lb_time := NULL] 
  
  # define new events by light barriers: in the line after all were turned off a new event starts.
  #ignore cases where a light barrier is turned ON or off twice
  #a transponder belongs to the upcoming event (defined by hardware), unless the time difference to the event after is tr_threshold seconds larger than to the row before; if it is more than 2*tr_threshold away from any LB, don't assign to LB activity.
  x[, lbo := 0]
  x[sensor == "lbo", lbo := ifelse(sensor_value == "ON" & shift(sensor_value[which(sensor == "lbo")], 1, "") != "ON", 1, lbo)]
  x[sensor == "lbo", lbo := ifelse(sensor_value == "OFF" & shift(sensor_value[which(sensor == "lbo")], 1, "") != "OFF", -1, lbo)]
  
  x[, lbi := 0]
  x[sensor == "lbi", lbi := ifelse(sensor_value == "ON" & shift(sensor_value[which(sensor == "lbi")], 1, "") != "ON", 1, lbi)]
  x[sensor == "lbi", lbi := ifelse(sensor_value == "OFF" & shift(sensor_value[which(sensor == "lbi")], 1, "") != "OFF", -1, lbi)]
  
  x[, event := 0]
  x[lbi != 0, event := lbi]
  x[lbo != 0, event := lbo]
  
  
  x[, event := cumsum(event)]
  x[, event := event - min(event)]
  
  x[, tra := (datetime_ - shift(datetime_,1,first(datetime_))+tr_threshold) < (shift(datetime_, 1, last(datetime_), type = "lead") - datetime_)]
  x[, event := ifelse(
      (event == 1 & shift(event, n = 1, fill = 0) == 0 & (shift(sensor, n=1, fill = "") != "tra" | shift(tra, 1, first(tra)) == TRUE)) |
      (sensor == "tra" & tra == FALSE & event == 0), 1, 0)]
  
  #also mark new event if the file id changes
  x[id != shift(id, 1, first(id)), event := event + 1]
  
  #mark new event if there is no activity for at least safety_threshold seconds
  x[datetime_ - shift(datetime_, 1, first(datetime_)) > safety_threshold & event == 0, event := event+1]
  
  x[, tra := NULL]
  
  x[, lbo := NULL]
  x[, lbi := NULL]
  
  #mark events based on time differences and light barriers
  x[, event := cumsum(event)]
  
  
  
  #fill in transponder for each event
  #if there are two transponders connected to one event, mark it as transp NA (because it is unclear whom it belongs to)
   x[sensor == "tra", multiple_tr := length(unique(sensor_value[which(sensor == "tra")])), by = event]
  x[sensor == "tra", transp := sensor_value]
  x[multiple_tr > 1 & sensor == 'tra', transp := as.character(paste0(unique(sensor_value[which(sensor == "tra")]), collapse = '___')), by = event]
  
 
  return(x)
  }

#' @export
assign_direction_v2 = function(x, hardware_threshold = 0.05) {
  #fetch time differences; takes long to calculate 
  x[, delta_ON := first(datetime_[which(sensor == 'lbo' & sensor_value == 'ON')]) - first(datetime_[which(sensor == 'lbi' & sensor_value == 'ON')]), by = event]
  x[, delta_OFF := last(datetime_[which(sensor == 'lbo' & sensor_value == 'OFF')]) - last(datetime_[which(sensor == 'lbi' & sensor_value == 'OFF')]), by = event]
  x[,delta_lbo := last(datetime_[which(sensor == 'lbo' & sensor_value == "OFF")]) - first(datetime_[which(sensor == 'lbo' & sensor_value == "ON")]), by = event]
  x[,delta_lbi := last(datetime_[which(sensor == 'lbi' & sensor_value == "OFF")]) - first(datetime_[which(sensor == 'lbi' & sensor_value == "ON")]), by = event]
  
  x[, startt := min(datetime_), by = event]
  x[, endt := max(datetime_), by = event]
  x[, r_pk := min(r_pk), by = event]
  
  x[, `:=`(datetime_ = NULL, sensor_value = NULL, sensor = NULL)]
  x = unique(x, by = names(x))
  x[, tmp := ifelse(duplicated(x[, event]) |  duplicated(x[, event], fromLast = TRUE), 1, 0)]
  x[tmp == 1, multiple_tr := unique(na.omit(multiple_tr))[1], by = event]
  x[is.na(multiple_tr), multiple_tr := 0]
  x[tmp == 1, transp := unique(na.omit(transp))[1], by = event]
  x[, tmp := NULL]
  x = unique(x, by = names(x))
  
  x[, event := NULL]
  x = unique(x, by = c(names(x)))
  
  
  #define direction based on delta light barrier turnon
  x[delta_ON <= -hardware_threshold , break_before := 1]
  x[delta_ON >= hardware_threshold , break_before := 0]
  x[delta_ON > -hardware_threshold & delta_ON < hardware_threshold, break_before := 0.5]
  x[is.na(delta_ON) & is.na(delta_lbo), break_before := 0]
  x[is.na(delta_ON) & is.na(delta_lbi), break_before := 1]
  x[is.na(delta_ON) & is.na(delta_lbi) & is.na(delta_lbo), break_before := 0.5] 
  
  
  #define direction based on delta light barrier turnoff
  x[delta_OFF < -hardware_threshold , break_after := 0]
  x[delta_OFF > hardware_threshold , break_after := 1]
  x[delta_OFF >= -hardware_threshold & delta_OFF <= hardware_threshold, break_after := 0.5]
  x[is.na(delta_OFF) & is.na(delta_lbo), break_after := 0]
  x[is.na(delta_OFF) & is.na(delta_lbi), break_after := 1]
  x[is.na(delta_OFF) & is.na(delta_lbi) & is.na(delta_lbo), break_after := 0.5] 
  
  #if multiple transponders are connected to one event, duplicate this event once for each transponder and set break_before and break_after to 0.5.
  x = x[rep(seq_len(nrow(x)), ifelse(multiple_tr==0,1,multiple_tr)),]
  x[multiple_tr > 1, break_before := 0.5]
  x[multiple_tr > 1, break_after := 0.5]
  x[multiple_tr > 1, transp := strsplit(unique(transp), "___"), by = r_pk]
  
  return(x)
  }

#' @export  
concat_events_v2 = function(x) {
  
  #combine events that belong together to new events
  x[, event := as.numeric(cumsum(ifelse(break_before+shift(break_after,1,0.5) >= 1, 1, 0))), by = 'transp']
  
  x[break_before == 0, dir1 := "I"]
  x[break_before == 0.5, dir1 := "?"]
  x[break_before == 1, dir1 := "O"]
  x[break_after == 0, dir2 := "I"]
  x[break_after == 0.5, dir2 := "?"]
  x[break_after == 1, dir2 := "O"]
  
  #if the next/previous event's direction is clear, mark accordingly with lower-case letters
  x[, dir1 := ifelse(dir1 == "?" & shift(dir2, 1, '') == "I", "i", dir1), by = transp]
  x[, dir1 := ifelse(dir1 == "?" & shift(dir2, 1, '') == "O", "o", dir1), by = transp]
  x[, dir2 := ifelse(dir2 == "?" & shift(dir1, 1, '', type = 'lead') == "I", "i", dir2), by = transp]
  x[, dir2 := ifelse(dir2 == "?" & shift(dir1, 1, '', type = 'lead') == "O", "o", dir2), by = transp]
  
  
  
  x[, `:=`(in_ = first(startt), 
           out_ = last(startt), 
           in_duration = round((first(endt) - first(startt)), digits = 3), 
           out_duration = round((last(endt) - last(startt)), digits = 3), 
           in_r_pk = first(r_pk), 
           out_r_pk = last(r_pk), 
           val1 = first(dir1),
           val2 = first(dir2),
           val3 = last(dir1),
           val4 = last(dir2)),
    by = list(transp, event)]
  x[in_r_pk == out_r_pk, ':=' (out_ = NA, out_duration = NA, out_r_pk = NA, val3 = NA, val4 = NA)]
  
  x[, validity := ifelse(!is.na(val3),
                         paste(paste(val1, val2, sep = '/'), paste(val3, val4, sep = '/'), sep = '|'),
                         paste(val1, val2, sep = '/'))]
  
  x[, ':=' (r_pk = NULL, delta_ON = NULL, delta_OFF = NULL, delta_lbo = NULL, delta_lbi = NULL, startt = NULL, endt = NULL, event = NULL, break_before = NULL, break_after = NULL, multiple_tr = NULL, dir1 = NULL, dir2 = NULL, val1 = NULL, val2 = NULL, val3 = NULL, val4 = NULL)]
  
  x = unique(x, by = c(names(x)))
  setkey(x, box, in_, out_, transp)
  return(x)

  }

#' @export
combine_front_v2 = function(x, threshold = 1.5){
  #NA transp to ''
  x[is.na(transp), transp := '']
  
  #mark new if time difference is larger than threshold
  x[, combine := ifelse((in_ - shift(ifelse(!is.na(out_), out_, in_), 1, first(in_)) - shift(ifelse(!is.na(out_duration), out_duration, in_duration),1,0)) > threshold, 1, 0)]

  #mark new if transp changes (to no tr or between trs)
  x[!is.na(transp), combine := ifelse(transp != shift(transp, 1, first(transp)), 1, combine) ]
  x[is.na(combine), combine := 1]
  x[toupper(validity) != 'O/O|O/O' | shift(toupper(validity), 1, '') != 'O/O|O/O', combine := 1]
  x[, combine := cumsum(combine)]
  
  x[, ':=' (in_ = first(in_), in_duration = first(in_duration), in_r_pk = first(in_r_pk), 
            out_ = last(out_), out_duration = last(out_duration), out_r_pk = last(out_r_pk),
            validity = ifelse(nchar(last(validity)) > 3, 
              paste(substring(first(validity), 1, 3), substring(last(validity), 5, 7), sep = '|'),
              first(validity))), by = combine]
  
  x[, combine := NULL]
  x = unique(x, by = names(x))
  
  x[, `:=`(in_, as.POSIXct(in_, origin = "1970-01-01"))]
  x[, `:=`(out_, as.POSIXct(out_, origin = "1970-01-01"))]
  x[transp == '', transp := NA]
  
  }
