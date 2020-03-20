

#' @title        Output diagnostics on raw SNB files (v2)
#' @description  Output diagnostics of a raw box file
#' @return       A data.table
#' @export
#' @author       MV
#' @examples     
#' \dontrun{
#' options(host = 'scidb.mpio.orn.mpg.de') 
#' filePath = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2018/2018.04.03/1/BOX0001.TXT"
#'           x = diagnose_raw_txt_v2(filePath); x; x
#' }

diagnose_raw_txt_v2 <- function(filePath) {

    x = readRaw_v2(filePath)
    box = basename2box(filePath) %>% empty2NA

    if(nrow(x) > 1) {
        
        hid = hwid(filePath)$hwid

        x = readRaw_v2(filePath)
        x[, k := 1:nrow(x)]

        e = x[str_detect(V, 'ERROR'), .(V, k) ]
        e[, err := str_split(V, 'ERROR', simplify = TRUE)[2], by = k ]
        e = e[!duplicated(err)]
        e = paste(e$err, collapse = ';')

        brc  = x[str_detect(V, 'Battery Remaining Capacity')][k == max(k), V]  %>% empty2NA
        bre  = x[str_detect(V, 'Battery Runtime to empty')][k == max(k), V] %>% empty2NA
        bav  = x[str_detect(V, 'Battery Actual Voltage')][k == max(k), V] %>% empty2NA
        sinc = x[str_detect(V, 'Time Synchronization')][k == max(k), V] %>% empty2NA
        rfid = x[str_detect(V, 'RFID is powered ON')][k == max(k), V] %>% empty2NA

        lastr = tail(x, 100)[, tr := str_extract(V, 'Transponder:[ \\t]*([^\\n\\r]*)') ][ !is.na(tr), .(tr)] %>% unique 
        lastr = paste(lastr$tr, collapse = ';')


    # diagnose output
        o = data.table(
                box  = box,
                HW_ID = hid,
                empty_file        =              'N',
                distinct_errors    =              e,
                last_Battery_Remaining_Capacity = brc,
                last_Battery_Runtime_to_empty =   bre ,
                last_Battery_Actual_Voltage  =    bav ,
                last_Time_Synchronization =       sinc,
                last_RFID_is_powered_ON =         rfid,
                last_transponders_1000rows =      lastr
            )


        } else

        o = data.table(box = box, empty_file = 'Y')



        o


    }


#' @title        Diagnose directory of one pull (i.e. a directory)
#' @description  Output diagnostics of a directory containing raw box files
#' @param   date           a character vector formated as 'yyyy.mm.dd' 
#'                         (same as the directory date holding the raw data)
#' @param   filenam       default to latest_SNB_diagnose.csv.
#' @param   outDirLocation the location of the raw data; the default is 
#'                         getOption('path.to.raw_v2')
#' 
#' @return       a data.table
#' @importFrom   parallel makePSOCKcluster detectCores stopCluster
#' @importFrom   doParallel registerDoParallel 
#' @export
#' @author       MV
#' @examples
#' \dontrun{
#' options(host = 'scidb.mpio.orn.mpg.de')  
#' x = diagnose_pull_v2()
#' }

diagnose_pull_v2 <- function(date, filenam='latest_SNB_diagnose.csv', outDirLocation = getOption('path.to.raw_v2') ) {

    if(missing(date)) {
        x = paste0(outDirLocation, year(Sys.time()) , sep = '/') 
        x = system( paste0('ls -td -- ',x, '/* | head -n 1'), intern = TRUE )
        date = basename(x)
        message('using latest date found: ', date)
    }

    path = paste0(outDirLocation, paste(year(char2date(date)), date, sep = '/') )

    cl = makePSOCKcluster(detectCores()); registerDoParallel(cl); on.exit( stopCluster(cl) )

    message( paste('Running diagnostics on raw data (v2) for', date) )

    ff = list.files(path, full.names = TRUE, recursive = TRUE, pattern = 'BOX\\d{4}.TXT|BOX\\d{4}.txt')

    # run diagnose_raw_txt for all files
        o = foreach( i = 1:length(ff), .packages= c('sdb', 'SNB2'), .errorhandling = 'remove')  %dopar% {
            diagnose_raw_txt_v2(ff[i])
        }

    x = data.table(box =path2box(ff) )
    setorder(x, box)
    o = rbindlist(o, fill = TRUE)
    o = merge(x, o, by = 'box', all.x = TRUE)
    o[is.na(empty_file), empty_file := 'possibly a corrupted file!!']
    o[, this_pull := 'yes']

    b = data.table(box =  getOption('boxes_v2') )

    o = merge(b,o, by = 'box', all.x = TRUE)

     o[is.na(this_pull), this_pull := 'file not present!']

    setorder(o, box)

    o[, date := date]


        
    data.table::fwrite(o, filenam) 




  }


