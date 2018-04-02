

#' @title        Box activity
#' @description  Box activity estimate based on light-barrier to transponder ratio  
#' @param        lastday a Date object  
#' @return       A data.table
#' @export
#' @author       MV
#' @examples     
#' \dontrun{
#'    x = box_activity()
#'    require(ggplot2)   
#'    require(ggrepel)   
#'   
#'   ggplot(x, aes(x = log(total_tr_reads+1), y = log(total_lb_reads), color = `N_transp\ndetected`  ) ) +
#'   geom_point() + geom_abline(intercept = 0, slope = 1) + 
#'   geom_text_repel(data =x[unk_act > .5], 
#'    aes(x = log(total_tr_reads+1), y = log(total_lb_reads), label = box) ) +
#'    xlab('log [ total transponder reads]') + 
#'    ylab('log [ total light-barrier reads]') + 
#'     scale_colour_brewer(palette = "Set1")

#' 
#'  }            

box_activity <- function(lastday, days_before = 1) {
    
    u =  getOption("DB_user") 
    h = getOption("host")
    h = 'scidb.mpio.orn.mpg.de'
    p = getOption("path.to.raw_v2")
    y = year(Sys.Date()) 
    db = getOption("snbDB_v2")
    
    if(missing(lastday)) {
        lastday = 
        data_dirs(p)$dir %>%
        char2date %>%
        max
    }

    #-----------------------------------
    con = dbcon(u, host = h, db = db)

    test.tr = tetr(con)

    a = Sys.time()
    d = boxes()
    d[, sql :=  paste('select sensor, sensor_value v FROM', box, 'WHERE datetime_ 
            BETWEEN', shQuote(lastday-days_before), 'AND', shQuote(lastday)) ]
    d = d[, dbq(con, sql), by = box]
    timetaken(a)

    d = merge(d, test.tr, by.x = 'v', by.y = 'transponder', all.x = TRUE, sort = FALSE)
    d = d[is.na(author) & sensor %in% c('tra', 'lbo') , .(v, box)]
    d = d[v != 'OFF']

    # N transponders
    X = d[v != 'ON', .N, .(box, v)][, .(n_tr = .N, total_tr_reads = sum(N)), by = box]
    # total lb reads
    x = d[v == 'ON', .(total_lb_reads = .N),  box]

    X = merge(x, X, all.x = TRUE, by = 'box')
    X[is.na(total_tr_reads), total_tr_reads := 0]
    X[is.na(n_tr), n_tr := 0]
    X[, box := b2int(box)]
    

    X[, unk_act := resid( lm(log(total_lb_reads) ~ log(total_tr_reads+1) ) ) ]
    X[unk_act < 0, unk_act := 0]
    X[, `N_transp\ndetected` := factor(n_tr)]



    X

}