# ==========================================================================
#  table level pipeline.
    # 1) fetch last entries in b000
    # 2) find new files to upload
    # 3) parse txt files
    # 4) load to db if ok else load path to black_list
# ==========================================================================

#' @title          txt Box files (txt) to db b000 tables pipeline
#' @param u       user, default to getOption("DB_user")
#' @param h       host, default to getOption("host")
#' @param p       path, default to getOption("path.to.raw_v2")
#' @param y       year, default to year(Sys.Date()
#' @param db      db name, default to getOption("snbDB_v2")
#' @return        TRUE if all txt are updated
#' @author        MV
#' @export
#' @examples
#' \dontrun{
#'  require(SNB2)
#'  demo_setup()
#'  scidb_snbUpdater()
#' }
#' 
scidb_snbUpdater <- function() {
  
    cat(' ------> Getting settings ...')  
        u =  getOption("DB_user") 
        h = getOption("host")
        p = getOption("path.to.raw_v2")
        y = year(Sys.Date()) 
        db = getOption("snbDB_v2")
        bb = getOption('boxes_v2')

        cat('db set to', dQuote(db) , '...done\n')


        #con = dbcon(user = u , host = h); on.exit( closeCon(con) )

    cat(' ------> Searching for proper directory formats ...')
        well_formated_directory(p, y); cat('done\n')

    cat(paste(' ------> Getting file listing on ', p , '...' ), '\n')
        ff = data_files(); cat('got', nrow(ff), '.done\n')

    cat(paste0(' ------> Getting files already on ', db , '.b000 ... ' ), '\n')
        uf = dbqSNB(u, h, q = 'select distinct path FROM boxtables', .boxes = bb)

    cat(' ------> Identifying new files ...')
        if(nrow(uf) == 0) nf = copy(ff) else
        nf = merge(ff, uf, by = 'path', all.x = TRUE)
        cat('got', nrow(nf), 'new files.\n')

    cat(' ------> Parsing new txt files ....')    

        O = foreach(i = 1: nrow(ff),.packages = c('SNB2') )  %do% {
            read_boxtxt(ff[i,p])      
          } ; cat('done\n')

    cat(' ------> Find if there are black listed files ....')    
        B = lapply(O, function(x) attributes(x)$SNB2 )
        B = rbindlist(B)
        B = B[garbage > 0.5, .(path)]

        if(nrow(B) > 0) {
            cat('got', nrow(B), 'bad files. will write to db ... \n')

        con = dbcon(u, host = h, db = db)
        dbWriteTable(con, 'black_list' , B , row.names = FALSE, append = TRUE)
        dbDisconnect(con)

            
        }
        



    # find black listed files




    cat(' ------> Updating b000 tables on',dQuote(db),'....  \n ')    









  # incoming_files
   cat(paste(' ------> Searching for incoming files (not yet in file_status) on ', p , '...' ), '\n')

    x = incoming_files(con, p, y, db)

    if( is.null(x)) {
      cat('     There are no new incoming files. \n       file_status table stays unchanged.\n')
      }

  # file_status_update1
    if( !is.null(x)) {
        cat(' ------> Updating file_status table ...', '\n')
        file_status_update1(con, x)
      }

  # hot_files
    cat(' ------> Getting the list of new txt files from the file_status table ...', '\n')
    h = hot_files(con, p, db)
    if( nrow(h) == 0) {
      
      cat('   There are no new files to upload. Will stop now.\n')
      stop('There are no new files to upload.')

      }

  # load_clean_txt
    cat(' ------> Fetching and preparing new files. This can take a while.... \n')
    

    dat = load_clean_txt_v2(h)


  # update_bTables
    cat(' ------> Updating individual box tables ....  \n ')
    if(length(dat))
    o = update_bTables(con, dat, db = db)

    return(o)


    cat(' ------ DONE ------ \n ')

 }








