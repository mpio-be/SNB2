
#' @title          txt Box files (txt) to db b000 tables pipeline
#' @return        TRUE if all txt are updated
#' @author        MV
#' @export
#' @examples
#' \dontrun{
#'  require(SNB2)
#'  
#'  demo_setup(install.test.db = TRUE, admin.user = 'mihai')
#'  scidb_snbUpdater()
#' }
#' 
scidb_snbUpdater <- function() {
    Start = Sys.time()
    cat(' ------> Getting settings ...')  
        u =  getOption("DB_user") 
        h = getOption("host")
        p = getOption("path.to.raw_v2")
        y = year(Sys.Date()) 
        db = getOption("snbDB_v2")
        bb = getOption('boxes_v2')

        cat('db set to', dQuote(db) , '...OK\n')


        #con = dbcon(user = u , host = h); on.exit( closeCon(con) )

    cat(' ------> Searching for proper directory formats ...')
        well_formated_directory(p, y); cat('OK\n')

    cat(' ------> Getting the file listing on ', p , '...')
        rawf = data_files(); cat('got', nrow(rawf), 'raw files.\n')

    cat(' ------> Getting files already on ', db , '.b000 ... ' )
        con = dbcon(u, host = h, db = db)

        dbf = data.table(box = int2b(bb) )
        dbf[, sql :=  paste('select distinct path FROM', db, '.', box ) ]
        dbf = dbf[, dbq(con, sql), by = box]

        cat('got', nrow(dbf), ' files listed in DB.\n')
        
    cat(' ------> Getting black listed files ' )
        blf = dbq(con, paste0('select distinct path from ',db, '.black_list')  )
        cat('got', nrow(blf), 'black list files.\n')

        alldbf = rbind(dbf[, .(path)], blf)
        alldbf[, indb := TRUE]
        alldbf[, path := paste0(p, path)]

        dbDisconnect(con)
   


    cat(' ------> Identifying new files ...')
        if(nrow(alldbf) == 0) newf = copy(rawf) else {

            newf = merge(rawf, alldbf, by = 'path', all.x = TRUE)
            newf = newf[is.na(indb)]
        }
        
        if( nrow(newf) == 0 ) stop ('-------- NO NEW FILES FOUND -------- ')

        cat('got', nrow(newf), 'new files. OK\n')

    cat(' ------> Parsing new txt files ....')    

        O = foreach(i = 1: nrow(newf),.packages = c('SNB2') )  %do% {
            read_boxtxt(newf[i,path])      
          } ; cat('OK\n')

    cat(' ------> Find if there are black listed files ....')    
        B = lapply(O, function(x) attributes(x)$SNB2 )
        B = rbindlist(B)
        B = B[garbage > 0.5, .(path)]

        if(nrow(B) > 0) {
            cat('got', nrow(B), 'bad files. will write to black_list ... \n')

        con = dbcon(u, host = h, db = db)
        dbWriteTable(con, 'black_list' , B , row.names = FALSE, append = TRUE)
        dbDisconnect(con)
        } else cat('All files are OK... \n')
        

    cat(' ------> Updating b000 tables on', dQuote(db),'.... ')    
        pb = txtProgressBar(max = length(O), style = 3 )
        con = dbcon(u, host = h, db = db)

        o = foreach(i = 1: length(O), .combine = c, .final=sum)  %do% {
            oi = O[[i]]
            atr = attributes(oi)$SNB2

            if(atr$garbage < 0.5) {
              res = dbWriteTable(con, int2b(atr$box) , oi , row.names = FALSE, append = TRUE)
              } else res = FALSE
            setTxtProgressBar(pb, i)
            res
        } 
        
        dbDisconnect(con)
        cat('\n      Uploaded', o , 'new files.\n')

    cat(' ------> Done in', timetaken(Start))

 }








