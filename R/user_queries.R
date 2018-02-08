
tetr <- function(con ) {
  dbq(con, 'select author, transponder from (
    select max(start) date_, initials author, transponder from FIELD_BTatWESTERHOLZ.AUTHORS where transponder is not NULL group by initials, transponder
    UNION
    select max(start) date_, initials author, transponder from BTatWESTERHOLZ.AUTHORS where transponder is not NULL group by initials, transponder ) x
    group by author, transponder')
}




#' @title        read file status
#' @description  read a subset of the the file status  given a date
#' @param        con a db connection
#' @return       a data.table; authors tests transponders are returned as well
#' @author       MV
#' @examples     con = dbcon('mihai', host = 'scidb.mpio.orn.mpg.de')
#' read.fstatus(con, "2016.02.17")

read.fstatus <- function(con, date) {

  x = dbq(con, paste("select * from SNBatWESTERHOLZ.file_status where date(datetime_) = date(", shQuote(date), ")") )
  tt = tetr(con)

  o = merge(x, tt, by = 'author')
  o

  }

#' query raw data, all b_ tables at once
#' @param   username       username
#' @param   host           host
#' @param   q              query,boxtables kw should be used instead of bnnn box name.
#' @param   testtr_remove  default to TRUE
#' @export
#' @examples
#' # Simple select
#' dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de", 'SELECT * FROM boxtables limit 1')
#'
#' # Last entry
#' dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de", 'SELECT * FROM boxtables ORDER BY r_pk desc limit 1')
#'
#' # Select last n days
#' x = dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de",
#' 'SELECT datetime_, LB, transp FROM  boxtables
#'    WHERE datetime_ >= DATE_ADD( (select max(datetime_) from  boxtables) , INTERVAL -3 DAY) AND
#'    YEAR(datetime_) = YEAR(CURDATE() )
#' ')
#'
dbqSNB <- function(username, host, q = 'SELECT * FROM boxtables limit 1', db = getOption('snbDB'), testtr_remove = TRUE, ncores = 4) {
    pb = tempfile(fileext = '.txt')
    message('to follow progress open', sQuote(pb), 'in a text editor')

    require(doParallel)
    cl = makePSOCKcluster(ncores); registerDoParallel(cl); on.exit(stopCluster(cl))

    x =  boxes()
    x[, q:= str_replace_all(q, 'boxtables', box) ]

   O = foreach(i = 1: nrow(x),.packages = c('sdb', 'SNB') )  %dopar% {
       cat(i,',', sep = '', file = pb, append = TRUE)
       con = dbcon(username, host = host); on.exit(dbDisconnect(con))
       dbq(con, paste('USE', db ) )
       o = dbq(con, x[i, q] )
       if(is.null(o) ) message(x[i, box] , 'returns no data for the given query.') else
       o[, box := x[i, box] ]
       o
    } %>% rbindlist


   if(nrow(O) == 0)  stop('Empty dataset.')

    O[, box := str_replace(box, 'b', '') %>% as.integer]

    if(testtr_remove) {
      ttr = dbq(user = username, host = host, q = 'select * from SNBatWESTERHOLZ.test_transponders')
      O = O[!transp%in%ttr$transponder]

    }

    O
}


#' overnight
#' @param   username username
#' @param   host     host
#' @param   date     date, default to last date in file_status table
#' @param   buffer  (hours)   sunrise + buffer
#' @examples
#' overnight(buffer = 2)
#'
overnight <- function(username = getOption('DB_user') , host = getOption('host'),buffer = 1, date = Sys.Date()-1 ) {

  if(missing(date))
    date = dbq(user = username, host = host, q = 'select max(datetime_) x from SNBatWESTERHOLZ.file_status')$x %>%
          as.Date

  # version 1
  x = dbqSNB(username, host = host,paste('
    SELECT DISTINCT datetime_, transp FROM  boxtables
      WHERE  LB <> "00" AND datetime_ BETWEEN', shQuote(date-1), 'AND', shQuote(date + 1) ) )

  # version 2
  x2 = dbqSNB(username, host,paste('
    SELECT DISTINCT datetime_, sensor_value transp FROM  boxtables
      WHERE  datetime_ BETWEEN', shQuote(date-1), 'AND', shQuote(date + 1) ),  db = getOption('snbDB_v2') )
  x2[transp %in% c('OFF', 'ON'), transp := NA]

  x = rbind(x, x2)
  enhanceOutput(x)

  x[, hour := hour(datetime_)  ]
  x[, day  := yday(datetime_)   %>% factor %>% as.numeric]

  # sun rise/set
  s = x[, mean(datetime_), by = day]
  s[, srise := maptools::sunriset(cbind(10.883864, 48.145393) , V1,  direction="sunrise", POSIXct.out = TRUE)$time]
  s[, sset  := maptools::sunriset(cbind(10.883864, 48.145393) , V1 , direction="sunset", POSIXct.out  = TRUE)$time][,V1:= NULL]
  s[, srise := srise + buffer*3600]
  x = merge(x, s, by = 'day')

  # subset given sunrise + buffer ad sunset
  x = x[ (day ==1 & datetime_ >= sset ) |  (day == 2 & datetime_ <= srise )  ][, ':=' (srise = NULL ,sset = NULL)]

  if(nrow(x) == 0)  stop('Empty dataset.')

  # transponder birds
  z = x[!is.na(transp), .(transp, box, day)] %>% unique

  z = dcast(z, transp +box ~ day, value.var = 'day')
  setnames(z, c('1', '2'), paste( c('d1', 'd2')))
  z = z[!is.na(d1) & !is.na(d2)]
  z

  # unknown birds
  u = x[!box%in%z$box ]
  u = dcast(u,box ~ day, value.var = 'day')
  setnames(u, c('1', '2'), paste( c('d1', 'd2')))
  u = u[d1 > 0 & d2 > 0]


  # output
  o = rbind(u, z, fill = TRUE)

  o = o[, .(box, transp)]
  setorder(o, box)
  o
  }








