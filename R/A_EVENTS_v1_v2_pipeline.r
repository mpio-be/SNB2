

#' @title Pipeline function to extract events
#'
#' @description Fetches the raw data using a connection to the database based on dbcon, then calculates when an individual was assumably at the nest box. Runs across the databases SNBatWESTERHOLZ and SNBatWESTERHOLZ_v2. Uses parallel computing.
#' @note Note that currently you need to have saved your credentials via save_credentials in order to run the function.
#' @param username the username that is supplied to dbcon for establishing the connection.
#' @param df a data.table that contains three columns about which data should be fetched: the box numbers, and the corresponding from and to (column names: box, from, to). See examples.
#' @param time_threshold_v1 time_threshold argument for function \link{events_v1}
#' @param max_distance_v1 max_distance argument for function \link{events_v1}
#' @param tr_threshold_v2 tr_threshold argument for function \link{events_v2}
#' @param cluster_events_threshold_v2 cluster_events_threshold argument for function \link{events_v2}
#' @param max_distance_v2 max_distance argument for function \link{events_v2}
#' @param cluster_fronts_threshold_v2 arguments for function \link{events_v2}
#' @param no_front_v2 no_front argument for function \link{events_v2}
#' @author LS
#' @export
#' @examples
#' df = data.table(box = 1, from = "2016-05-01 00:00:00", to = "2019-06-30 00:00:00")
#' hooray = eva(YOUR_USER_NAME_as_specified_in_saveCredentials, df, time_threshold_v1 = 10, tr_threshold_v2 = 100)
#' hooray
#' plot(hooray)
#' plot(subset(hooray, !is.na(transp) & box == 1 & in_ > "2019-03-15" & in_ < "2019-04-05"))
#' #Note that you can access the colours and counts of individual transponders by using
#' tr = plot(hooray)
#' tr

eva = function(username, df, setTZ = "Etc/GMT-2",
               time_threshold_v1 = 2, max_distance_v1 = 16*60*60,
               tr_threshold_v2 = 5, cluster_events_threshold_v2 = 2, max_distance_v2 = 16*60*60, cluster_fronts_threshold_v2 = 5, no_front_v2 = NULL) {
  library(doParallel)
  cl = makePSOCKcluster(50) # 50 cores
  registerDoParallel(cl)
  
  d = foreach(i = 1:nrow(df)) %dopar% {
    require(SNB2)
    con = dbcon(username)
    raw_x.v1 = dbq(con, paste0("SELECT * FROM SNBatWESTERHOLZ.", int2b(df[i, box]), " where datetime_ > '", df[i,from], "' and datetime_ < '", df[i,to], "'"))
    raw_x.v2 = dbq(con, paste0("SELECT * FROM SNBatWESTERHOLZ_v2.", int2b(df[i, box]), " where datetime_ > '", df[i,from], "' and datetime_ < '", df[i,to], "'"))
    closeCon(con)
    L = list()
    if(nrow(raw_x.v1) > 1) { x.v1 = events_v1(raw_x.v1, setTZ = setTZ, time_threshold = time_threshold_v1, max_distance = max_distance_v1, silent = TRUE); if(!is.null(x.v1)) {x.v1[, type := "v1"]; x.v1[, id := NULL] }; L[[length(L)+1]] = x.v1 }
    if(nrow(raw_x.v2) > 1) { x.v2 = events_v2(raw_x.v2, setTZ = setTZ, tr_threshold = tr_threshold_v2, cluster_events_threshold = cluster_events_threshold_v2, max_distance = max_distance_v2, cluster_fronts_threshold = cluster_fronts_threshold_v2, no_front = no_front_v2, silent = TRUE); if(!is.null(x.v2)) {x.v2[, type := "v2"]; x.v2[, path := NULL] }; L[[length(L)+1]] = x.v2  }
    
    if(length(L) == 0) return() else {
      x = rbindlist(L); x[, box := df[i, box]];return(x) 
  }
  stopCluster(cl)
  registerDoSEQ()
  }
  
  d = rbindlist(d)
  class(d) = c("SNBoutput", class(d))
  print(paste0("Timezone: ", setTZ))
  d
}
  
  
  