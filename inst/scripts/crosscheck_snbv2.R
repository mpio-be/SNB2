
# Cross-check video scoring - SNB v2 raw data

#+ Tools, settings, message = FALSE, echo = FALSE
  pckg = sapply(c('sdb', 'SNB', 'data.table', 'magrittr', 'stringr',  'ggplot2' ,'knitr'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ) )
    stopifnot(all(pckg))

    fn = '/ds/raw_data_kemp/FIELD/Westerholz/SNB/CALIBRATION/VIDEO/2016/total.finalized.videoanalysis.txt'
    con = dbcon('valcu', host = "scidb.mpio.orn.mpg.de")

    round_to_mins <- function(x, y){
      x = as.POSIXlt(x + as.difftime((y/2), units="mins"))
      x$sec = 0
      x$min<- y*(x$min %/% y)
      as.POSIXct(x)
      }

    ROUND = 1 # mins

#+  Data
    # Video
    v = read.table(fn, sep = ' ', stringsAsFactors = FALSE) %>% data.table
    v = v[, .(datetime_,box,event,transponder,partner_inside,hole_event)]
    v[, box := paste0('b',str_pad(box, 3, 'left', pad = '0'))]
    v[, datetime_ := as.POSIXct(datetime_)]
    v[, day := yday(datetime_)]
    v[, dt_round := round_to_mins(datetime_, ROUND)] # round to  y mins

    # SNB
    x = v[, .(first = min(datetime_), last = max(datetime_)), by = .(box)]
    x[, sql := paste0('SELECT * from SNBatWESTERHOLZ_v2alpha.',box,
                        ' WHERE datetime_
                            BETWEEN ', shQuote(first), ' AND ', shQuote(last)), by = box]
    s = x[, dbq(con, sql), by = box ]
    s[, datetime_ := as.POSIXct(datetime_)]
    s[, day := yday(datetime_)]

    # subset to each video and day
    x = v[, .(start_rec = min(datetime_), stop_rec = max(datetime_)), by = .(box, day)]
    s = merge(s, x, by = c('box', 'day'))
    s = s[datetime_ >= start_rec & datetime_ <= stop_rec]

    # round datetime
    s[, dt_round := round_to_mins(datetime_, ROUND)]

#+ VIDEO & SNB data merged by the rounded time
    vs = merge(
        x = v[, .(dt_round, box, event, partner_inside, hole_event, transponder)],
        y = s[, .(dt_round, sensor_value, box, sensor)],
        by  = c('box', 'dt_round'),
        all.y = TRUE, all.x = TRUE, allow.cartesian = TRUE)
    vs = unique(vs)


    kable( head(vs), caption =
        paste('VIDEO & SNB data merged by', ROUND, 'minute(s).
        Columns 2 through 5 are obtained from the video files,
        columns 6 through 8 are SNB data '))


# CHECK 1: test transponder
    x = vs[, no_test_tr_read := !is.na(transponder) & is.na(sensor_value)  ]
    x = vs[!is.na(transponder)]
    o = x[, .(Test_transp_not_read = sum(no_test_tr_read), N  = .N), by = .(box)]
    kable(o)


# CHECK 2: overall match rate [an event triggers any sensor]
    o = vs[, .( box_does_not_read = sum(is.na(sensor_value)), all_events = .N), by = .(box)]
    o[, pct_non_reads := box_does_not_read/all_events*100]
    kable(o)


# CHECK 3: transponder reader [bird inside should trigger a transponder read]
    o = vs[is.na(transponder)  & (sensor == 'tra' | is.na(sensor) ), .( box_does_not_read = sum(is.na(sensor_value)), all_events = .N), by = .(box)]
    o[, pct_non_reads := box_does_not_read/all_events*100]
    kable(o)

    o = vs[is.na(transponder)  & (sensor == 'tra' | is.na(sensor) ), .( box_does_not_read = sum(is.na(sensor_value)), all_events = .N),
            by = .(box, hole_event)]
    o[, pct_non_reads := box_does_not_read/all_events*100]
    kable(o)


 # A copy of this file is bounded with the SNB package, run `system.file('scripts', 'crosscheck_snbv2.R', package = 'SNB')` to get it.



