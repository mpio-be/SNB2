
#' @title system screening: looks up all the relevant system settings
#' @param pulldate Sd card pool date
#' @param password password
#' @return data.table with 3 columns
#' @author MV
#' @export
#' @examples
#' head(system_status(password = 'some char'))
system_status <- function(pulldate = Sys.Date(), password = '') {

  # parameters
    dpath       = dirname( getOption('path.to.raw_v2') )
    repo        =  abbrPath(dpath)
    good_reader = getOption('cardReader')
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
#' @export
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

#' @title       looks up the box number 
#' @description looks up the box number in BoxNNNN.txt and optionally cross-checks it with the HW-ID 
#' @param       path       file path
#' @param       boxid      data.table with current box - hwid
#' @param       hwidCheck  check hwid
#' @return      data.table containing Boxnummer.txt info
#' @export
#' @author      MV
#' @examples
#'  require(sdb)
#'  boxid <- harwareIDs()
#' x = cardReader()
#' z = x[, read.boxnumber(mountpoint, boxid = boxid), by = .(row.names(x)) ]
read.boxnumber <- function(path, boxid, hwidCheck = TRUE) {

  nfiles = length(list.files(path))

  
    if(nfiles == 0) { # empty card
      v = NA
      b = NA
      empty = TRUE
      valid = FALSE
      }

  if(nfiles > 1)  # dirty card
    stop( paste('An SD card with ', nfiles, 'files. Proceed with caution ☃.') )


  if( nfiles == 1) { # ok card

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

  data.table(box = as.integer(b), valid, empty)

  }

#' @title               Checks inserted SD cards.
#' @param download_date default to Sys.Date()
#' @param x             a DT as returned by cardReader() for file_copy_status
#'                      or a DT returned by file_copy_status() for card_copy_status()
#' @param ...           pass to  read.boxnumber, specifically hwidCheck
#' @return              a DT
#' @author MV
#' @export
#' @examples
#'  require(sdb)
#'  bid <- harwareIDs()
#'  x = file_copy_status(boxid = bid)
#'  card_copy_status(x)
file_copy_status <- function(download_date = Sys.Date() , x = cardReader(), ...) {

  x[, id := .(row.names(x))]
  b = x[, read.boxnumber(mountpoint, ...), by = id ]

  x = merge(x, b, by = 'id', all.x = TRUE)

  src = x[,  .(src = list.files(mountpoint,full.names = TRUE) ) , by = id ]
  x = merge(x, src , by = 'id', all.x = TRUE)

  x[, dest := paste0(paste0(getOption("path.to.raw_v2"), '/',year(download_date), '/'),
              format(download_date, '%Y.%m.%d') ,'/', box, '/', basename(src)) ]

  x[is.na(src), dest := NA]

  x

 }

#' @rdname file_copy_status
#' @export
card_copy_status <- function(x = file_copy_status () ) {

  # collapse by sdcard
  cc = x[, .(nfiles = length( which(!is.na(src))) ), by = .(id, ro, kname, box,  valid, empty)]
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
#' @export
#' @examples
#'  require(sdb)
#'  boxid <- harwareIDs()
#'  x = file_copy_status(bid = boxid)
#'  sdcard_uploader(x)
sdcard_uploader <- function(x) {
  ccs = card_copy_status(x)
  z = merge(x, ccs[, .(box, copied)], by = 'box', allow.cartesian = TRUE)

  z = z[ (valid)  & !(copied)]

  if(nrow(z) == 0) stop('nothing to do')

  cl = makePSOCKcluster(4); registerDoParallel(cl)
  o = foreach(i = 1:nrow(z) , .packages = 'SNB2', .combine = c)  %dopar% {
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
#' @export
#' @examples
#' x = file_copy_status(bid = boxid)
#' sdcard_uploader(x) # copy
#' scard_cleaner(x)   # empty files on cards when copied
scard_cleaner <- function(x) {

    ccs = card_copy_status(x)
    w = merge(x, ccs[, .(box, copied)], by = 'box',allow.cartesian = TRUE)

    vv = w[(copied) ]

    if( nrow(vv) > 0) {
        foreach(i = 1:nrow(vv) )  %do% {
        for (n in 1:10) file.copy('/dev/null', vv[i,src], overwrite = TRUE)
        }
      }

  }

#' @title    Checks inserted SD cards.
#' @param    x       a DT as returned by cardReader()
#' @param    boxid   data.table with current box -hwid
#' @return   data.table
#' @author   MV
#' @export
#' @examples
#' card_prepare_status()
card_prepare_status <- function( x = cardReader() ){

  x[, id := 1:nrow(x)]
  x[, is.empty := list.files(mountpoint) %>% length == 0 , by = id ]

  #html messages
  x[(is.empty)  , html := Span('Empty card.', 'success', 'open-file')]
  x[!(is.empty) , html := Span('☠Non-empty card☠', 'warning', 'warning-sign')]
  x[ro == '1'   , html := Span("Read only card.", 'danger', 'danger-sign') ]

  x
  }

#' @title Prepare SD cards
#' @param x  card_prepare_status() output
#' @param password  password
#' @author MV
#' @export
sdcard_prepare <- function(x = card_prepare_status (), ids) {

  if(nrow(x) == 0) stop('Nothing to do.')
  
  if( !all(x$is.empty) ) stop('🙊 Hey! you cannot prepare non-empty cards!!')

  if(nrow(x) > length(ids) ) stop('More cards than id-s.')
  if(nrow(x) < length(ids) ) stop('Less cards than id-s.')

  x[, box := ids]
  x[, file_id := paste0(mountpoint, '/BOX',str_pad(box, 4, 'left', 0), '.TXT')]
  x[, syscall := paste('echo        ""       >', shQuote(file_id) ), by = id]

  # RUN system call to make an empty file
  o = x[, system(syscall), by = id]

  nrow(o[V1 == 0])

  }


#' @title Format card
#' @author MV
#' @export
format2vfat <- function(mountpoint, filesystem, password) {

  if( !find_installed('mkfs.fat') ) stop('dosfstools is not installed')

    o = system('id -u  $USER', intern = TRUE) %>% paste0('uid=', .)
    o = paste0(o, ',rw,umask=000')

    # unmount
    system( paste('echo', shQuote(password), '| sudo -S umount' , mountpoint), wait = TRUE)
    system( paste('echo', shQuote(password), '| sudo -S rmdir' ,  mountpoint), wait = TRUE )

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
#' @export
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
#' @export
sdcard_format <- function(password, x = card_format_status () ) {

  pwdCheck = check_sys_pwd(password)
  if( !pwdCheck ) stop('Password does not match.')

  if(nrow(x) == 0) stop('Nothing to do.')

  o = x[, format2vfat(mountpoint, filesystem, password = password), by = id]
    if(any(o$V1 > 0)) stop(" I could not format all or some cards. Re-insert the cards and try again.")

  nrow(o[V1 == 0])

  }




