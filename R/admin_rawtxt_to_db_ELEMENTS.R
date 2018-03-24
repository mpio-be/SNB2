

#' @title            drop ALL data associated with a given ID
#' @description      The function removes all lines in both file_status and box tables associated with a given id.
#' @param            id the \code{id}-s in file status and the associated box tables.
#' @param            con a connection object
#' @author           MV
#' @export
drop_by_id <- function( con, id , db = getOption("snbDB_v2") ) {

    dbq(con, paste('USE', db  ))

    k = dbq(con,  paste('select id, path, box from file_status WHERE id in (', paste(id, collapse = ','), ')')    )

    N_dropped_from_file_status = dbExecute(con, paste('DELETE FROM file_status WHERE id in (', paste(id, collapse = ','), ')') )

    k[, N_removed_from_box_tables := dbExecute(con, paste('DELETE FROM', int2b(box),  'WHERE id =', id) )  ,  by = id   ]

    message(paste(
        nrow(k) , 'box tables were updated ( N = ', sum(k$N_removed_from_box_tables), 'rows deleted)' , 
        "&", N_dropped_from_file_status, 'lines removed from file_status') )


    k[, ':=' (id = NULL, N_removed_from_box_tables = NULL)]
    k[, path := paste0(getOption('path.to.raw_v2'), path)]

    k

    }


#' @title              Check the raw data directory 
#' @description        The function checks the proper directory format: \code{ year/date[prefix]/box/b00x.txt}
#' @param       p      path to raw data, default to getOption("path.to.raw")
#' @return             logical
#' @author             MV
#' @export
#' @examples
#' \dontrun{
#' well_formated_directory(getOption("path.to.raw_v2"))
#' }
well_formated_directory <- function(p = getOption("path.to.raw_v2") , y = year(Sys.Date()) ) {

    x = data.table( dirs = list.files( paste0(p, y), full.name = TRUE) )

    x[, dirnam := basename(dirs) ]

    isNotDate = x[ ! grepl('^[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}$', dirnam )   ]

    if(nrow(isNotDate) > 0) {
        return(isNotDate)
        stop('Invalid directories')
    } else return(TRUE)

    }


#' @title         Find out new data files 
#' @description   The function lists new files which are not yet in  \code{SNBatWESTERHOLZ)v2.file_status}.
#' @section       Warning: Before performing the file listing the function only looks for new directories therefore any
#'                    files which are copied to a given directory AFTER a db update will be overlooked!
#' @param p       path to raw data, default to getOption("path.to.raw")
#' @param y       year, default to current year
#' @param con     a connection object
#' @return        a data.table if there are new directories or otherwise NULL
#' @author        MV
#' @export
#' @examples 
#' \dontrun{
#' require(SNB2)
#' con = dbcon('snbAdmin')
#' incoming_files(con)
#' dbDisconnect(con)
#' }
incoming_files <- function( con, p = getOption("path.to.raw_v2") , y =year(Sys.Date()), db = getOption("snbDB_v2") ) {

    dbq(con, paste('USE', db))

    newdirs = data.table( dirs = list.files( paste(p, y, sep ='/'), full.name = TRUE, recursive = TRUE) )
    newdirs = newdirs[grep('^box.{1,4}.txt$', tolower(basename(dirs)), perl = TRUE), ]
    newdirs[, dirs   := gsub('/box.{1,4}.txt$', '', dirs, ignore.case = TRUE) ]
    newdirs[, dirnam := gsub(p, '', dirs) ]


    fileStatus = dbq(con, 'SELECT path from file_status' )
    if(nrow(fileStatus) == 0) fileStatus = data.table(path = character(0))
    fileStatus[, dirnam := dirname(path) ]
    fileStatus = unique(subset(fileStatus, select = 'dirnam'), by = 'dirnam')

    newdirs = newdirs[ ! newdirs$dirnam %in% fileStatus$dirnam,  ]
    out = newdirs[, .(path = list.files(dirs, full.names = TRUE, recursive = TRUE))  ]

    if(nrow(out) == 0) out = NULL else {
        out = out[ grep('^box.{1,4}.txt$', tolower(basename(path)), perl = TRUE) ]
        out[, box := sapply(str_split(path, '/'), function(x) x[length(x) -1] ) ]
        out[, box := int2b(as.integer(box))]
        setorder(out, box)
        comment(out) = c(p, db)
        }

    out
    }


#' @title           File_status table: initial update
#' @description     box, id, path,  filesize are saved to file_status table.
#'                  The id is a primary key in file_status and identifies chunks of data from the same file in each box b00x table.
#' @param con       a connection object
#' @param  x        a table exported by the incoming_files
#' @param update    default to TRUE
#' @return          TRUE or the update content when update = FALSE
#' @author          MV
#' @export
#' @examples
#' \dontrun{
#' require(SNB2)
#' con = dbcon('snbAdmin')
#' x = incoming_files(con )
#' o = file_status_update1(update = FALSE)
#' dbDisconnect(con)
#' }
file_status_update1 <- function(con, x, update = TRUE) {

    x_is_file_status_file = all(inherits(x, 'data.table') , names(x) %in% c('path', 'box') , length(comment(x)) == 2 )

    if( !x_is_file_status_file ) stop('x must be a data.table created by incoming_files()')

    snbDB        = comment(x)[2]
    path.to.raw  = comment(x)[1]
    comment(x) <- NULL

    x = data.table(x)

    dbq(con, paste('USE', snbDB  )  )
    cn = dbq(con, 'SELECT * FROM file_status where FALSE') %>% names

    x[, filesize    :=  file.size(x$path)/1024  ]
    x[, path        := str_replace(path, path.to.raw, "") ]
    x[, datetime_     := char2date(path) ]


    # add missing column names
    for(nam in setdiff(cn, names(x)) ) set(x, j = nam, value = NA)

    setcolorder(x, cn)

    # add box
    x[, box := as.integer(str_replace(box, 'b', ''))]


    if(update)
    o = dbWriteTable(con, 'file_status', x, row.names = FALSE, append  = TRUE) else o = x

    return(o)

    }


#' @title           Hot txt files
#' @description     Files listed in \code{file_status} table (upload_status is NULL) which are ready to process and upload
#' @param con       A connection object
#' @return          \code{data.table}
#' @author          MV
#' @export
hot_files <- function(con, p = getOption("path.to.raw_v2"), db = getOption("snbDB_v2") ) {

    dbq(con, paste('USE', db  )  )

    o = dbq(con, "SELECT id, box, path from file_status where upload_status is NULL")
    o[, path:= paste0(p, path)]
    comment(o) = c(p, db)
    o

    }


#' @title           Load clean txt files 
#' @description     Load all files which are free of bugs and thus directly importable to 
#'                  a \code{data.table} with attribute
#' @param con       A connection object
#' @param h         An output of \code{\link{hot_files}}   containing box and path
#' @note            The attribute of each \code{data.table} is another one-row \code{data.table}
#'                  with 3 columns: \code{box}, \code{id} (corresponding with the file_status id) 
#'                  and \code{upload_status} = 1 (clean load).
#'                  # the function will try to run in parallel. 
#' @return          a list of \code{data.table} with attributes named 'snb'.
#' @author          MV
#' @export
load_clean_txt_v2 <- function(h) {
    
    if( require(doParallel) & nrow(h) > 2 ) { # do we go parallel? 
    
        all_cores = parallel::detectCores()
        if(all_cores > nrow(h)  ) ncores = nrow(h) else ncores = all_cores -1

        cl = makePSOCKcluster(ncores)
        registerDoParallel(cl)
        on.exit(stopCluster(cl))
        on.exit(registerDoSEQ())
        
        }

        o = foreach(i=1:nrow(h), .packages = c('SNB2'), .errorhandling = 'pass') %dopar% {

            x = read_snb_txt_v2(h[i,]$path)

            # set attributes
            aa = h[i, .(id, box)]
            aa[, upload_status := 1]
            setattr(x, 'snb', aa)
            x
        }

     o[sapply(o, FUN = inherits, what = 'data.table')]
    }


#' @title           Update  box and file_status tables
#' @description     Update  box tables with parsed txt data (e.g. by \code{load_clean_txt_v2}  )
#' @param con       a connection object
#' @param dat       a list of \code{data.table}-s with attributes (see \code{load_clean_txt_v2} ).
#' @return          TRUE when both boxes (all) and  file status was updated else FALSE.
#' @author          MV
#' @export
update_bTables <- function(con, dat, db = getOption("snbDB_v2") ) {
    dbq(con, paste('USE', db  ))

    bar  = txtProgressBar(min = 1, max = length(dat),  style = 3, width = floor(getOption("width")/2)  )

    o = foreach(i = 1:length(dat), .combine = all) %do% {

        di = dat[[i]]
        aa = attributes(di)$snb
        di[, id := aa$id]
        box = int2b(aa$box)

        setTxtProgressBar(bar, i)

        write_data = dbWriteTable(con, box, di, row.names = FALSE, append = TRUE)
        if(write_data)
        id_is_updated = dbExecute( con,
            paste('UPDATE file_status set upload_status =',aa$upload_status, ',
                    dt_loaded =' ,shQuote(Sys.time()),
                       'where id =' , aa$id) )

        write_fs = id_is_updated == 1

        all(write_fs, write_data)

        }

    return(o)    
        
        cat( paste('\n', length(dat), 'files uploaded to DB\n') )


    }




