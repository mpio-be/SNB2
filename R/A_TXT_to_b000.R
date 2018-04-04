
#' @title          txt Box files (txt) to db b000 tables pipeline
#' @return        TRUE if all txt are updated
#' @author        MV
#' @export
#' @examples
#' \dontrun{
#'  require(SNB2)
#'  
#'  demo_setup(install.test.db = TRUE, admin.user = 'mihai')
#'  scidb_snbUpdater.b000 ()
#' }
#' 
scidb_snbUpdater.b000 <- function(file = '~/scidb_snbUpdater.log') {
    
    if(interactive() ) OF = '' else OF = file

    Start = Sys.time()
    cat(' ------> Getting settings ...', append=TRUE, file=OF) 
        u =  getOption("DB_user") 
        h = getOption("host")
        p = getOption("path.to.raw_v2")
        y = year(Sys.Date()) 
        db = getOption("snbDB_v2")
        bb = getOption('boxes_v2')

        cat('db set to', dQuote(db) , '...OK\n', append=TRUE, file=OF)


        #con = dbcon(user = u , host = h); on.exit( closeCon(con) )

    cat(' ------> Searching for proper directory formats ...', append=TRUE, file=OF)
        well_formated_directory(p, y); cat('OK\n', append=TRUE, file=OF)

    cat(' ------> Getting the file listing on ', p , '...', append=TRUE, file=OF)
        rawf = data_files(); cat('got', nrow(rawf), 'raw files.\n', append=TRUE, file=OF)

    cat(' ------> Getting files already on ', db , '.b000 ... ' , append=TRUE, file=OF)
        con = dbcon(u, host = h, db = db)

        dbf = data.table(box = int2b(bb) )
        dbf[, sql :=  paste('select distinct path FROM', db, '.', box ) ]
        dbf = dbf[, dbq(con, sql), by = box]

        cat('got', nrow(dbf), ' files listed in DB.\n', append=TRUE, file=OF)
        
    cat(' ------> Getting black listed files ' , append=TRUE, file=OF)
        blf = dbq(con, paste0('select distinct path from ',db, '.black_list')  )
        cat('got', nrow(blf), 'black list files.\n', append=TRUE, file=OF)

        alldbf = rbind(dbf[, .(path)], blf)
        alldbf[, indb := TRUE]
        alldbf[, path := paste0(p, path)]

        dbDisconnect(con)
   
    cat(' ------> Identifying new files ...', append=TRUE, file=OF)
        if(nrow(alldbf) == 0) newf = copy(rawf) else {

        newf = merge(rawf, alldbf, by = 'path', all.x = TRUE)
        newf = newf[is.na(indb)]
        }

        if( nrow(newf) == 0 ) {
            cat('None found. Will stop now.\n', append=TRUE, file=OF)
            return(0)
            stop ('-------- NO NEW FILES FOUND -------- ')

        } 

        cat('got', nrow(newf), 'new files. OK\n', append=TRUE, file=OF)

    cat(' ------> Parsing new txt files ....', append=TRUE, file=OF )

        O = foreach(i = 1: nrow(newf)  )  %do% {
            read_boxtxt(newf[i,path])      
          } ; cat('OK\n', append=TRUE, file=OF)

    cat(' ------> Find if there are black listed files ....', append=TRUE, file=OF)
        B = lapply(O, function(x) attributes(x)$SNB2 )
        B = rbindlist(B)
        B = B[garbage > 0.5, .(path)]

        if(nrow(B) > 0) {
            cat('got', nrow(B), 'bad files. will write to black_list ... \n', append=TRUE, file=OF)

        con = dbcon(u, host = h, db = db)
        dbWriteTable(con, 'black_list' , B , row.names = FALSE, append = TRUE)
        dbDisconnect(con)
        } else cat('All files are OK... \n', append=TRUE, file=OF)
        

    cat(' ------> Updating b000 tables on', dQuote(db),'.... ', append=TRUE, file=OF)
        pb = txtProgressBar(max = length(O), style = 3 )
        con = dbcon(u, host = h, db = db)

        out = foreach(i = 1: length(O), .combine = c, .final=sum)  %do% {
            oi = O[[i]]
            atr = attributes(oi)$SNB2

            if(atr$garbage < 0.5) {
              res = dbWriteTable(con, int2b(atr$box) , oi , row.names = FALSE, append = TRUE)
              } else res = FALSE
            setTxtProgressBar(pb, i)
            res
        } 
        
        dbDisconnect(con)
        cat('\n      Uploaded', out , 'new files.\n', append=TRUE, file=OF)

    cat(' ------> Done in', timetaken(Start), append=TRUE, file=OF)

    out


 }








