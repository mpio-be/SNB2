
#' @title Fetch data from database
#' @description This function fetches the raw data from the database using either datetime_ or r_pk as a reference column.
#'
#' @param con An open connection
#' @param box The box number from which to collect data
#' @param from Defines the starting point at which to fetch data. This can refer either to the r_pk column (numeric)
#' or can be a date and time (in the format "YYYY-MM-DD HH:MM:SS")
#' @param to Defines the end point to which to fetch data. This can refer either to the r_pk column (numeric)
#' or can be a date and time (in the format "YYYY-MM-DD HH:MM:SS")
#'
#'
#' @author LS
#' @export
#' @examples #see function events
#' @seealso \code{\link{events}}, \code{\link{remove_faulty_data}}, \code{\link{trim_data}},
#' \code{\link{fetch_ins_outs}}, \code{\link{assign_direction}}, \code{\link{concat_events}},
#'

fetch_data_v1 = function(con, box = NA, from = NA, to = NA) {#supply from and to can either be pks or datetimes
.Defunct
}


#' @title Split data into different pieces of activity for SNB data version v1
#' @description This function fetches individual in and out events based on a time threshold and transponder readings.
#' If two transponder readings occur close in time, the rows in between are removed, because it is unclear to which
#' individual this data belongs, and the information about the direction of the movement is marked as "unclear".
#'
#' @param time_threshold Time in seconds. If the time difference between two consecutive lines is larger or equal to time_threshold, the rows will be treated as independent pieces of activity. Defaults to 2.
#' @details See \link{events_v1} for further information.
#' @export
#' @examples
#' #Not run
#' #see help of function events_v1
#' ?events_v1()

fetch_ins_outs_v1 = function(x, time_threshold) {
  make_names_local(x)
  #make sure all columns can be interpreted
  x = x[!is.na(datetime_),]
  x = x[!is.na(LB),]
  
  #in the early data, LBs may still be coded differently
  x[LB == 0, LB := "00"]
  x[LB == 1, LB := "10"]
  x[LB == 2, LB := "02"]
  x = subset(x, LB %in% c('10', '00', '12', '02'))
  x[, bout_length := NULL]
  x[, bv := NULL]
  setkey(x, id, datetime_, r_pk)
  if(nrow(x) > 0) {
  
  #a. remove_data is a vector which defines which rows should be removed or kept. This vector is defined in the following piece of code. This part is meant to remove any data that is superfluous and is in there for reasons of efficiency.
  x = x[shift(LB, type = 'lead') == "12" | shift(LB, type = 'lag') == "12" |
            !is.na(transp) | !is.na(shift(transp, type = 'lead')) | !is.na(shift(transp, type = 'lag')) |
            LB == "12" | shift(datetime_, type = 'lead') != datetime_ | shift(datetime_, type = 'lag') != datetime_,]
  #b. mark events by time difference backup
  
  if(nrow(x) > 0) {
    x[, event := datetime_ - shift(datetime_, fill = datetime_[1])]
    x[, event := ifelse(event >= max(time_threshold,2) | event < -3, 1, 0)]
    x[, tmp_event := cumsum(event)]
    x[, next_tr := 0]
    x[, prev_tr := 0]
    x[!is.na(transp), next_tr := ifelse(transp == shift(transp, fill = last(transp), type = 'lead'), 0, 1), by = tmp_event]
    x[!is.na(transp), prev_tr := ifelse(transp == shift(transp, fill = transp[1], type = 'lag'), 0, -1), by = tmp_event]
    x[, next_tr := ifelse(shift(next_tr, fill = 0) == 1, 1, 0)]
    x[prev_tr == -1 | next_tr == 1, event := 1]
    #mark the lines between transponders for deletion
    x[, delete := ifelse(next_tr == 1, 1, 0)]
    x[, event := cumsum(event)]
    x[, delete := max(delete), by = event]
    x = x[delete == 0,]
    x[,  ':=' (delete = NULL, next_tr = NULL, prev_tr = NULL, tmp_event = NULL)]
    }
  }
  return(x)
}




#' @title Assign direction to different pieces of activity
#'
#' @description Assign direction to different pieces of activity, e.g. "IN" and "OUT"
#'
#' @param x A data.table as supplied by \link{fetch_ins_outs_v1}.
#' @details See \link{events_v1} for further information. Note that after some minor alterations, the output of this function is returned, if in function \link{events_v1} groups_ins_and_outs = FALSE.
#' @export
#' @examples
#' #Not run
#' #see help of function events_v1
#' ?events_v1()

assign_direction_v1 = function(x) {
  make_names_local(x)
  x[LB == "00", ":=" (break_before_tmp = as.character(NA), break_after_tmp = as.character(NA))]
  x[LB != "00", break_before_tmp := first(LB), by = event]
  x[LB != "00", break_after_tmp := last(LB), by = event]
  x[, break_before_tmp := na.omit(break_before_tmp)[1], by = event]
  x[, break_after_tmp := na.omit(break_after_tmp)[1], by = event]
  x[, break_before := 0.5]
  x[break_before_tmp == "02", break_before := 0]
  x[break_before_tmp == "10", break_before := 1]
  x[, break_after := 0.5]
  x[break_after_tmp == "02", break_after := 0]
  x[break_after_tmp == "10", break_after := 1]
  x[, ':=' (break_before_tmp = NULL, break_after_tmp = NULL)]
  x[, r_pk := first(r_pk), by = event]
  x[, startt := min(datetime_), by = event]
  x[, endt := max(datetime_), by = event]
  x[,datetime_:=NULL]
  
  #remove FRONT and BACK without corresponding transponder read
  x[, transp := na.omit(transp)[1], by = event]
  x[, keep := ifelse(max(LB) == "12" | !is.na(transp), 1, 0), by = event]
  x = x[keep==1,]
  x[, keep := NULL]
  x[,LB:= NULL]
  x = unique(x)
  x[,event := NULL]
  
  
  return(x)
}


#' @title Combind ins and outs to events
#' @description This function combines individual instances at the nestbox to nestbox presence and absence data, where possible.
#' The function aims to be conservative; it aims to combine two (or more) events only if they are extremly likely to belong together.
#' Functions that can be used for more precise, but also less conservative data can be found elsewhere (tba).
#'
#' @param x a data.frame as provided by the function assign_direction
#'
#' @return a data.frame ins and outs grouped together to single events, where possible.
#'
#' @details In order to avoid loops, this function defines for each direction available whether before or after this
#' line a new event should begin. For example, "IN" and "FRONT" always start an event (break added before this line),
#' OUT always stops an event (break added after this line). "PASSES" may be grouped together with INs and OUTs, but
#' not with other PASSES. Events where the bird did not pass through the entrance are not considered to begin or end
#' events per se.
#'
#' @note Rule definitions are marked in the code with a "###RULE###".
#'
#' @author LS
#' @export
#' @examples #see function events
#' @seealso \code{\link{events}}, \code{\link{fetch_data}}, \code{\link{remove_faulty_data}},
#' \code{\link{trim_data}}, \code{\link{fetch_ins_outs}}, \code{\link{assign_direction}},
#'
#'

concat_events_v1 = function(x, max_distance) {
  make_names_local(x)
  x[is.na(transp), transp := '']
  x[, event := as.numeric(cumsum(ifelse(
    (break_before+shift(break_after,1,0.5) >= 1) | (startt - shift(startt, 1, startt[1])) > max_distance,  1, 0))), by = 'transp']
  
  #classify side bird started/finished action
  # dir1: based on order, in which light barriers turned on, i.e. initial light barrier
  x[break_before == 0, dir1 := "I"]   # initial LB is LBI -> bird starts from inside = I
  x[break_before == 0.5, dir1 := "?"]# not clear which turned on first -> ? ###########changed!
  x[break_before == 1, dir1 := "O"]# initial LB is LBO -> bird starts from outside = O
  # dir2: based on order, in which light barriers turned off, i.e. final light barrier
  x[break_after == 0, dir2 := "I"]# final LB is LBI -> bird ends at inside = I
  x[break_after == 0.5, dir2 := "?"]# not clear which turned off first -> ?###########changed!
  x[break_after == 1, dir2 := "O"]# final LB is LBO -> bird ends at outside = O
  
  #if the next/previous action's side is clear, mark accordingly with lower-case letters
  x[, dir1 := ifelse(break_before == 0.5 & shift(dir2, 1, '') == "I", "i", dir1), by = transp]# i= previous inside
  x[, dir1 := ifelse(break_before == 0.5 & shift(dir2, 1, '') == "O", "o", dir1), by = transp]# o=previous outside
  x[, dir2 := ifelse(break_after == 0.5 & shift(dir1, 1, '', type = 'lead') == "I", "i", dir2), by = transp]# i=next is inside
  x[, dir2 := ifelse(break_after == 0.5 & shift(dir1, 1, '', type = 'lead') == "O", "o", dir2), by = transp]# o=next is outside
  
  
  
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


#' @export
translate_validity_v1 = function(x) {
  make_names_local(x)
  x[, direction_detail := validity]
  #make everything IN-OUT that isn't FRONT
  x[, direction := 'IN-OUT']
  #everything with 2 pieces of information is FRONT unless...
  x[nchar(direction_detail) == 3, direction := "FRONT"]
  #unless it is I/O (=OUT) or O/I (=IN)
  x[toupper(direction_detail) == "I/O", direction := "OUT"]
  x[toupper(direction_detail) == "O/I", direction := "IN"]
  x[, validity := NULL]
  CORRECT = c('O/O', 'O/I|I/O', '?/I|I/?', 'O/I|I/?', '?/I|I/O', '?/I|I/?')
  a = c(nrow(x[toupper(direction_detail) %in% CORRECT,])/nrow(x),
        nrow(x[toupper(direction_detail) %in% CORRECT & !is.na(transp),])/nrow(x[!is.na(transp),]))
  output = list(x, a)
  return(output)
}
