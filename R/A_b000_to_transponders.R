#' @title          b000 tables to transponders table pipeline
#' @return         TRUE on success
#' @author        MV
#' @export
#' @examples
#' \dontrun{
#'  require(SNB2)
#'  
#'  demo_setup(install.test.db = TRUE, admin.user = 'mihai')
#'  scidb_snbUpdater.b000()
#'  scidb_snbUpdater.transponders()
#' }
#' 

scidb_snbUpdater.transponders <- function(file = '~/scidb_snbUpdater.log') {

 if(interactive() ) OF = '' else OF = file

    Start = Sys.time()
    cat(' ------> Getting settings ...', file = OF, append = TRUE) 
        u   =  getOption("DB_user") 
        h   = getOption("host")
        p   = getOption("path.to.raw_v2")
        y   = year(Sys.Date()) 
        db  = getOption("snbDB_v2")
        tdb = getOption("transpondersDB")
        bb  = getOption('boxes_v2')

        cat('db set to', dQuote(db), 'transponders db set to', dQuote(tdb) , '...OK\n', file = OF, append = TRUE)

    con = dbcon(u, host = h, db = db); on.exit(dbDisconnect(con))


    cat(' ------> Getting the file list on ', db , '.b000 ... ' , file = OF, append = TRUE)

        box000f = data.table(box = int2b(bb) )
        box000f[, sql :=  paste('select distinct path FROM', db, '.', box ) ]
        box000f = box000f[, dbq(con, sql), by = box]

        cat('got', nrow(box000f), ' files listed in DB.\n', file = OF, append = TRUE)


    cat(' ------> Getting the file list on ', tdb , '.transponders ... ', sep = '' , file = OF, append = TRUE)

        transpf = dbq(con, paste0('select distinct path from ', tdb, '.transponders') )
        transpf[, done := 1]

        cat('got', nrow(transpf) , ' files listed in transponders and', file = OF, append = TRUE)

        newf = merge(box000f, transpf, by = 'path', all.x = TRUE)
        newf = newf[is.na(done)]
        cat(nrow(newf) , ' files to append to transponders ... \n', file = OF, append = TRUE)

    cat(' ------> Running INSERT INTO STATEMENTS for each b000 table ...', file = OF, append = TRUE)
        newf = newf[, .(path = paste( shQuote(path), collapse = ',') ), by = box]
        newf[, path := paste('(', path, ')')]
        newf[, boxno := b2int(box)]

        newf[, sql := paste(
            paste0('INSERT INTO ' , tdb, '.transponders' ,  ' (site_type, site, transponder,datetime_, path )'),
            "select 1 site_type,", boxno,  " site, sensor_value transponder, datetime_, path 
                from", box,
                "where sensor = 'tra' and path in ", path)]

        if( nrow(newf) > 0 )
        newf[, o := dbExecute(con, sql), by = box]

        cat(sum(newf$o) , 'lines inserted into transponders ... \n', file = OF, append = TRUE)


    cat(' ------> Done in', timetaken(Start), file = OF, append = TRUE)


    }

