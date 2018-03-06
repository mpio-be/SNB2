
#' @title     test transponders
#' @description the latest test transponder and their holders (authors)
#' @param     condb a db connection
#' @return    a data.table
#' @export
#' @author    MV
#' @examples  con = dbcon('mihai', host = 'scidb.mpio.orn.mpg.de')
#' tetr(con)

tetr <- function(condb ) {
  
  dbq(con = condb, 'SELECT author, transponder from (
    SELECT max(start) date_, initials author, transponder from FIELD_BTatWESTERHOLZ.AUTHORS 
      WHERE  transponder is not NULL 
        GROUP BY initials, transponder
          UNION
    SELECT max(start) date_, initials author, transponder from BTatWESTERHOLZ.AUTHORS 
      WHERE transponder is not NULL 
        GROUP BY initials, transponder ) x
  GROUP BY author, transponder')
  

  }




#' query raw data, all b_ tables at once
#' @param   username       username
#' @param   host           host
#' @param   q              query,boxtables kw should be used instead of bnnn box name.
#' @param   testtr_remove  default to TRUE
#' @export
#' @examples
#' # Simple SELECT
#' dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de", 'SELECT * FROM boxtables limit 1')
#'
#' # Last entry
#' dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de", 'SELECT * FROM boxtables ORDER BY r_pk desc limit 1')
#'
#' # SELECT last n days
#' x = dbqSNB('mihai', host = "scidb.mpio.orn.mpg.de",
#' 'SELECT datetime_, LB, transp FROM  boxtables
#'    WHERE datetime_ >= DATE_ADD( (SELECT max(datetime_) from  boxtables) , INTERVAL -3 DAY) AND
#'    YEAR(datetime_) = YEAR(CURDATE() )
#' ')
#'
dbqSNB <- function(username, host, q = 'SELECT * FROM boxtables limit 1', db = getOption('snbDB_v2'), .boxes = 1:277, ncores = 4) {

    pb = tempfile(fileext = '.txt')
    message('to follow progress open', sQuote(pb), 'in a text editor')

    require(doParallel)
    cl = makePSOCKcluster(ncores); registerDoParallel(cl); on.exit(stopCluster(cl))

    x =  boxes()[box %in% int2b(.boxes)]


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


    if(nrow(O) == 0)  warning('Your query returns an empty dataset.')

    if(nrow(O) > 0)   
    O[, box := str_replace(box, 'b', '') %>% as.integer]


    O
  }


#' overnight
#' @param   username username
#' @param   host     host
#' @param   date     date, default to last date in file_status table
#' @param   buffer  (hours)   sunrise + buffer
#' @export
#' @param   ...  goes to dbqSNB
#' @examples
#' overnight(buffer = 2)
#' x = overnight( date = anydate('2018.01.23') )
#'
overnight <- function(buffer = 1, date = Sys.Date()-1, ...) {

  if(missing(date))
    date = dbq(user = getOption('DB_user') , host = getOption('host'), db = getOption('snbDB_v2')
      , q = 'SELECT max(datetime_) x from file_status')$x %>%
          as.Date


  x = dbqSNB( getOption('DB_user') , getOption('host') ,
    q = paste('
    SELECT DISTINCT datetime_, sensor_value transp FROM  boxtables
      WHERE  datetime_ BETWEEN', shQuote(date-1), 'AND', shQuote(date + 1) ),  db = getOption('snbDB_v2'), ... )

  if(nrow(x) == 0) stop(paste('There are no data on', date))
  
  x[transp %in% c('OFF', 'ON'), transp := NA]
  enhanceOutput(x)
  x[, hour := hour(datetime_)  ]
  x[, day  := yday(datetime_)   %>% factor %>% as.numeric]

  # sun rise/set
  s = x[, mean(datetime_), by = day]
  s[, srise := maptools::sunriset(cbind(10.883864, 48.145393) , V1,  direction="sunrise", POSIXct.out = TRUE)$time]
  s[, sset  := maptools::sunriset(cbind(10.883864, 48.145393) , V1 , direction="sunset", POSIXct.out  = TRUE)$time][,V1:= NULL]
  s[, srise := srise + buffer*3600]
  x = merge(x, s, by = 'day')

  # subset given sunrise + buffer and sunset
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



#' hardwareIDs table
#' 
#' @export
harwareIDs <- function(username  = getOption('DB_user') , host = getOption('host') ) {

    con = dbcon(user =username, host = host,   db = getOption('snbDB_v2') )
    on.exit(dbDisconnect(con))
    
    o = dbq(con, 'select box, hwid from boxid where action = "start" order by box, datetime_ desc')[!duplicated(box)]
    if(nrow(o) < 277) warning("Not all boxes have a registered hardware ID!")

    o  

}




