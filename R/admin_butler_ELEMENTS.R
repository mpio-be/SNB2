
#' @title system screening: looks up all the relevant system settings
#' @param pulldate Sd card pool date
#' @param password password
#' @return data.table with 3 columns
#' @author MV
#' @examples
#' head(system_status(password = 'some char'))
system_status <- function(pulldate = Sys.Date(), password = '') {

  # parameters
    dpath       = dirname( options('path.to.raw')$path.to.raw )
    repo        =  abbrPath(dpath)
    good_reader = options('cardReader')$cardReader
    x           = cardReader()
    cr          = unique( str_trim(x$vendor) )
    os          = Sys.info()[1]

  # date
    if(Sys.Date() == pulldate) {
      msg_date =  'are you sure it is today?'
      w_date = 1
      } else {
      msg_date = paste('was', difftime(Sys.Date(), pulldate), "days ago.")
      w_date = 0
      }

  # file path
    if( file.exists(dpath))  {
      msg_repo = paste(repo, 'is accessible.')
      w_repo = 0
      } else {
      msg_repo =  paste(repo, 'is not accessible!')
      w_repo = 1
      }

  # cards and reader
   if( length(cr) == 0) {
    msg_cardReader = 'has no inserted SD cards.'
    w_cardReader = 1
   }
   if( length(cr) == 1 &&  cr != good_reader) {
    msg_cardReader = paste('is not', good_reader)
    w_cardReader = 1
   }
   if(length(cr) > 1) {
    msg_cardReader =  paste(length(cr), 'types detected.')
    w_cardReader = 1
   }
   if( length(cr) == 1 &&  cr == good_reader) {
    msg_cardReader = paste('is', good_reader)
    w_cardReader = 0
   }

  # os
   if(os == 'Linux') {
    msg_os =   paste0('is supported (', os, ')')
    w_os = 0
   } else {
    msg_os =   paste0('is not supported (', os, ')')
    w_os = 1
   }

  # output table
    o = data.table(
    focus_on = c('Pull date', 'Repository', 'Card reader', 'OS'),
    message  = c(msg_date, msg_repo, msg_cardReader, msg_os),
    w        = c(w_date, w_repo, w_cardReader, w_os) %>% as.character
    )

    o[w ==1, w := '<span class="glyphicon glyphicon-warning-sign" aria-hidden="true"></span>']
    o[w ==0, w := '<span class="glyphicon glyphicon-ok" aria-hidden="true"></span>']
    o


  }

#' @title Finds attached card reader devices (a wrapper to lsblk)
#' @return data.table containing info on HOTPLUG,MOUNTPOINT,KNAME,MODEL,VENDOR,TRAN,FSTYPE
#' @param   cardsize default to getOption('cardSize')
#' @author MV
#' @examples
#' x = cardReader();x
cardReader <- function(cardsize = getOption('cardSize') ) {
  if( !find_installed('lsblk') ) stop('lsblk is not installed')

   o = jsonlite::fromJSON( system('lsblk -J -oHOTPLUG,RO,MOUNTPOINT,KNAME,VENDOR,FSTYPE,SIZE',intern=TRUE) ) %$%
    blockdevices %>%
    data.table
  o[, vendor := zoo::na.locf(vendor, na.rm= FALSE) ]
  o[, fstype := zoo::na.locf(fstype, na.rm= FALSE) ]

  o[, size := suppressWarnings(gsub('G', '', size) %>% as.numeric) ]
  o = o[size > cardsize - cardsize * 0.15 & size <= cardsize]

  o = o[hotplug == 1 & fstype == 'vfat' ]

  o[, filesystem := paste0('/dev/',kname)]

  o = o[!is.na(mountpoint)]

  # card insert order
  if(nrow(o) < 2 && exists('cio', .GlobalEnv) ) rm('cio', envir = .GlobalEnv)
  if(exists('cio', .GlobalEnv) ) {
    x = get('cio', envir = .GlobalEnv)
    x =   c(cio,   setdiff(o$kname, cio)  )
  } else x = o$kname
  if(length(x) > 0) assign('cio', x, envir = .GlobalEnv)

  # re-order
  o[, kname := factor(kname, levels = x)   ]
  setorder(o, 'kname')
  o

  }

#' @title       looks up the box number (v1, v2)
#' @description looks up the box number in Boxnummer.txt (v1) or by association with HW-ID (v2)
#' @param       path       file path
#' @param       nam        default to Boxnummer.txt
#' @param       boxid      data.table with current box - hwid
#' @param       hwidCheck  check hwid
#' @return      data.table containing Boxnummer.txt info
#' @author      MV
#' @examples
#'  require(sdb)
#'  boxid <- dbq( q= 'select box, hwid from boxid order by box, datetime_ desc',
#'          db = getOption('snbDB_v2'), host = getOption('host'), user =getOption('DB_user'))[!duplicated(box)]
#' x = cardReader()
#' z = x[, read.boxnumber(mountpoint, boxid = boxid), by = .(row.names(x)) ]
read.boxnumber <- function(path, nam  = 'Boxnummer.txt', boxid, hwidCheck = TRUE) {

  nfiles = length(list.files(path))
  boxnrfile = paste(path, nam, sep = '/')

  # empty card
    if(nfiles == 0) {
      v = NA
      b = NA
      empty = TRUE
      valid = FALSE
      }

  # version 1
    if(file.exists(boxnrfile)) {
      v = 1
     if(nfiles >  2)  stop( paste('An SD card (v1) with ', nfiles, 'files. I do not know what to do.') )
     if(nfiles == 1)  empty = TRUE
     if(nfiles == 2)  empty = FALSE

      b =  readLines(con  = boxnrfile, warn = FALSE)

      if( length(b) == 0  || is.na(as.integer(b)) ) {
         b = NA
         valid = FALSE
         }

      if( length(b) > 0 && as.integer(b)  ) valid = TRUE

      }

  # version 2
    if(!file.exists(boxnrfile) & nfiles > 0) {
      v = 2
      if(nfiles > 1)  stop( paste('An SD card(v2) with ', nfiles, 'files. I do not know what to do.') )

      ff = list.files(path, full.names = TRUE)

      empty = file.info(ff, extra_cols = FALSE)$size < 2
      b = basename2box(ff)

      if(hwidCheck & !empty) {
        b_by_hwid = merge(hwid(ff), boxid, by = 'hwid')[, box]


        if(length(b_by_hwid) > 0 && b != b_by_hwid)
         stop( paste('HardwareID box', shQuote(b_by_hwid), 'different than filename box',shQuote(b) ) )
        }

      if( as.integer(b) > 0 ) {
        valid = TRUE
        }

      if( length(b) == 0 | !as.integer(b) ) {
        b = NA
        valid = FALSE
        }
      }

  data.table(box = as.integer(b), valid, empty, ver = as.integer(v))

  }

#' @title               Checks inserted SD cards.
#' @param download_date default to Sys.Date()
#' @param x             a DT as returned by cardReader() for file_copy_status
#'                      or a DT returned by file_copy_status() for card_copy_status()
#' @param bid           boxid DT (see GUI)
#' @return              a DT
#' @author MV
#' @examples
#'  require(sdb)
#'  boxid <- dbq( q= 'select box, hwid from boxid order by box, datetime_ desc',
#'          db = getOption('snbDB_v2'), host = getOption('host'), user =getOption('DB_user'))[!duplicated(box)]
#'  x = file_copy_status(bid = boxid)
#'  card_copy_status(x)
file_copy_status <- function(download_date = Sys.Date() , x = cardReader(), bid = boxid) {

  x[, id := .(row.names(x))]
  b = x[, read.boxnumber(mountpoint, boxid = bid), by = id ]

  x = merge(x, b, by = 'id', all.x = TRUE)

  src = x[,  .(src = list.files(mountpoint,full.names = TRUE) ) , by = id ]
  x = merge(x, src , by = 'id', all.x = TRUE)

  # ver 1
  x[ver == 1, dest := paste0(paste0(getOption("path.to.raw"),'/', year(download_date), '/'),
              format(download_date, '%Y.%m.%d') ,'/', box, '/', basename(src)) ]

  # ver 2
  x[ver == 2, dest := paste0(paste0(getOption("path.to.raw_v2"), '/',year(download_date), '/'),
              format(download_date, '%Y.%m.%d') ,'/', box, '/', basename(src)) ]

  x[is.na(src), dest := NA]

  x

 }

#' @rdname file_copy_status
card_copy_status <- function(x = file_copy_status () ) {

  # collapse by sdcard
  cc = x[, .(nfiles = length( which(!is.na(src))) ), by = .(id, ro, kname, box, ver, valid, empty)]
  cc[, box_padded := paste('Box', str_pad(box, pad = ' ', width = 3, side = 'both') ) ]

  # copied
  cc = merge(cc, x[, .(copied = all(file.exists(dest)) ), by = box] , by = 'box', sort = FALSE)

  # html msg
  cc[(valid)&(!copied)&(!empty),        msg := paste(box_padded, 'good to go.') %>% Span('success', 'open-file')    ]
  cc[(valid)&(copied)&(empty),          msg := paste(box_padded, 'done.')       %>% Span('info',    'glyphicon-ok') ]

  cc[(valid)&(!copied)&(empty),         msg := paste(box_padded, 'No data.')    %>% Span('danger', 'danger-sign') ]


  cc[(!valid) & nfiles > 0,          msg := 'This is not a valid SNB card'      %>% Span('danger', 'danger-sign')   ]
  cc[nfiles     == 0 ,               msg := "Empty anonymous card."             %>% Span('warning', 'warning-sign') ]
  cc[ro         == 1 ,               msg := "Read only card."                   %>% Span('warning', 'warning-sign') ]

  cc

 }

#' @title Copy SD cards content to repository
#' @param    x        file_copy_status output
#' @return TRUE on success
#' @author MV
#' @examples
#'  require(sdb)
#'  boxid <- dbq( q= 'select box, hwid from boxid order by box, datetime_ desc',
#'          db = getOption('snbDB_v2'), host = getOption('host'), user =getOption('DB_user'))[!duplicated(box)]
#'  x = file_copy_status(bid = boxid)
#' sdcard_uploader(x)
sdcard_uploader <- function(x) {

  ccs = card_copy_status(x)
  z = merge(x, ccs[, .(box, copied)], by = 'box', allow.cartesian = TRUE)

  z = z[ (valid)  & !(copied)]

  if(nrow(z) == 0) stop('nothing to do')

  cl = makePSOCKcluster(4); registerDoParallel(cl)
  o = foreach(i = 1:nrow(z) , .packages = 'SNB', .combine = c)  %dopar% {
    if( !dir.exists (dirname(z[i,dest])) ) dirname(z[i,dest]) %>% dir.create(recursive = TRUE)

    file.copy( z[i, src], z[i,dest], overwrite = FALSE, copy.date = TRUE)
      if( file.exists(z[i,dest] ) && (file.info(z[i, src])$size == file.info(z[i,dest])$size ) )
       return( unique(z[i]$box) ) else return(NULL)
  }

  stopCluster(cl)
  registerDoSEQ()

  # cclog
  write.table(unique(o), file = cclog(), append = TRUE,quote = FALSE, sep = '\n', row.names = FALSE, col.names = FALSE)


  }


#' @title         Remove data (overwrite using an empty file several times) and THEN delete content of the copied SD cards.
#' @param    x    file_copy_status output
#' @author MV
#' @examples
#' x = file_copy_status(bid = boxid)
#' scard_cleaner(x)
scard_cleaner <- function(x) {

    ccs = card_copy_status(x)
    w = merge(x, ccs[, .(box, copied)], by = 'box',allow.cartesian = TRUE)

    # version 1
    v1 = w[!grepl('Boxnummer.txt', basename(src)) & (copied) & ver == 1]

    if( nrow(v1) > 0) {
        foreach(i = 1:nrow(v1) )  %do% {
        for (n in 1:10)  file.copy('/dev/null', v1[i,src], overwrite = TRUE)
        file.remove(v1[i,src]  )
        }
      }

    # version 2
     v2 = w[(copied) & ver == 2]

    if( nrow(v2) > 0) {
        foreach(i = 1:nrow(v2) )  %do% {
        for (n in 1:10) file.copy('/dev/null', v2[i,src], overwrite = TRUE)
        }
      }

  }

#' @title    Checks inserted SD cards.
#' @param    x     a DT as returned by cardReader()
#' @param    boxid data.table with current box -hwid
#' @return   data.table
#' @author   MV
#' @examples
#' card_prepare_status()
card_prepare_status <- function( x = cardReader() ){

  x[, id := 1:nrow(x)]
  x[, is.empty := list.files(mountpoint) %>% length == 0 , by = id ]

  #html messages
  x[(is.empty)  , html := Span('Empty card', 'success', 'open-file')]
  x[!(is.empty) , html := Span('Non-empty card.  Format first.', 'warning', 'warning-sign')]
  x[ro == '1'   , html := Span("Read only card.", 'danger', 'danger-sign') ]

  x
  }

#' @title Prepare SD cards
#' @param x  card_prepare_status() output
#' @param password  password
#' @author MV
sdcard_prepare <- function(x = card_prepare_status (), ids) {

  if(nrow(x) == 0) stop('Nothing to do.')

  if(nrow(x) > length(ids) ) stop('More cards than id-s.')
  if(nrow(x) < length(ids) ) stop('Less cards than id-s.')

  x[, box := ids]

  # version
  x[box%in% getOption('boxes_v2'), ver := 2]
  x[is.na(ver), ver := 1]

  x[ver == 1, file_id := paste0(mountpoint, '/Boxnummer.txt')]
  x[ver == 2, file_id := paste0(mountpoint, '/BOX',str_pad(box, 4, 'left', 0), '.TXT')]


  # PREPARE: add file and box-id
  x[ver == 1, syscall := paste('echo',shQuote(box) ,'>', shQuote(file_id) ), by = id]
  x[ver == 2, syscall := paste('echo        ""       >', shQuote(file_id) ), by = id]


  # RUN system calls
  o = x[, system(syscall), by = id]

  nrow(o[V1 == 0])

  }


#' @title Format card
#' @author MV
format2vfat <- function(mountpoint, filesystem, password) {

  if( !find_installed('mkfs.fat') ) stop('dosfstools is not installed')

    o = system('id -u  $USER', intern = TRUE) %>% paste0('uid=', .)
    o = paste0(o, ',rw,umask=000')

    # unmount
    system( paste('echo', shQuote(password), '| sudo -S umount' , mountpoint), wait = TRUE)
    system( paste('echo', shQuote(password), '| sudo -S rmdir' , mountpoint), wait = TRUE )

    # format
    system( paste('echo', shQuote(password), '| sudo -S mkfs.fat' , filesystem), wait = TRUE )

    #mount
    # system( paste('echo', shQuote(password), '| sudo -S mkdir -p' , mountpoint), wait = TRUE ) # -p dir exists
    # system( paste('echo', shQuote(password), '| sudo -S mount -o',o , filesystem, mountpoint), wait = TRUE )

  }

#' @title    Checks inserted SD cards.
#' @param    x     a DT as returned by cardReader()
#' @param    boxid data.table with current box -hwid
#' @return   data.table
#' @author   MV
#' @examples
#' card_format_status()
card_format_status <- function( x = cardReader() ){

  x[, id := 1:nrow(x)]
  x[, is.empty := list.files(mountpoint) %>% length == 0 , by = id ]

  #html messages
  x[(is.empty)  , html := Span('Empty card, ready to format.', 'success', 'open-file')]
  x[!(is.empty) , html := Span('Card is not empty.', 'warning', 'warning-sign')]
  x[ro == '1'   , html := Span("Read only card.", 'warning', 'warning-sign') ]

  x
  }


#' @title            Format SD cards
#' @param x          card_format_status() output
#' @param password   password
#' @author MV
sdcard_format <- function(x = card_format_status (), ids, password, format = FALSE) {

  pwdCheck = suppressWarnings( system(paste('echo' ,shQuote(password) ,  '| sudo -S echo 1'),
                intern = TRUE, ignore.stderr = TRUE) ) %>% length
  if(pwdCheck == 0) stop('Password does not match.')

  if(nrow(x) == 0) stop('Nothing to do.')

  o = x[, format2vfat(mountpoint, filesystem, password = password), by = id]
    if(any(o$V1 > 0)) stop(" I could not format all or some cards. Something funny is happening ...")

  nrow(o[V1 == 0])

  }





cclog <- function() {
  paste0(tempdir(), '/cclog.txt')
 }

### HTML  ---------------------------------------------------------------------

#' @title html span
#' @author MV
#' @examples span('text')
Span <- function(x, label = 'primary', glyphicon = 'open-file', div = TRUE, h = 2) {
  o = paste0('<span class="label label-',label,'"><i class="glyphicon glyphicon-',glyphicon,'"></i>', x, '</span>  ')
  if(div) {
    h = paste0('h', h)
    o = paste0('<div><',h,'>', o, '</',h,'></div>' )
    }
  o
  }

options_footer <- function(style = "position: absolute; bottom: 0; left: 1; font-size:12px") {
  div(class = "col-sm-12 text-left text-muted",style=style,
        HTML('<hr>',
            '<strong>Package options:</strong><br>',
            paste('<strong>host:</strong>', getOption('host'), '<br>'),
            paste('<strong>raw data:</strong>', paste(getOption('path.to.raw') %>% str_sub(., 1, 18), "..."),'<br>' ),
            paste('<strong>db name:</strong>', getOption('snbDB'), '<br>'),
            paste('<strong>db user:</strong>', getOption('DB_user') , '<br>'),
            paste('<strong>package:</strong> SNB', packageVersion('SNB') )
         )
      )

  }
