

#' @title        Output diagnostics on raw SNB files (v2)
#' @description  Output diagnostics of a raw box file
#' @return       A data.table
#'
#' @author       MV
#' @seealso \code{\link{diagnose_raw_dir}}
#' @examples     filePath = "/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2/2017/2017.03.03/122/BOX0122.TXT"
#' 				 x = diagnose_raw_txt_v2(filePath); x; x

diagnose_raw_txt_v2 <- function(filePath) {

	x = readRaw_v2(filePath)
	hid = hwid(filePath)$hwid
	box = basename2box(filePath) %>% empty2NA

	if(nrow(x) > 1) {
		# raw data
		x = readRaw_v2(filePath)
		x[, k := 1:nrow(x)]

		e = x[str_detect(V, 'ERROR'), .(V, k) ]
		e[, err := str_split(V, 'ERROR', simplify = TRUE)[2], by = k ]
		e = e[!duplicated(err)]
		e = paste(e$err, collapse = ';')

		brc  = x[str_detect(V, 'Battery Remaining Capacity')][k == max(k), V]
		bre  = x[str_detect(V, 'Battery Runtime to empty')][k == max(k), V]
		bav  = x[str_detect(V, 'Battery Actual Voltage')][k == max(k), V]
		sinc = x[str_detect(V, 'Time Synchronization')][k == max(k), V]
		rfid = x[str_detect(V, 'RFID is powered ON')][k == max(k), V]


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
				last_RFID_is_powered_ON =         rfid
			)


		} else

		o = data.table(box = box, empty_file = 'Y')



		o


	}


#' @title        Diagnose directory of one pull (i.e. a directory)
#' @description  Output diagnostics of a directory containing raw box files
#' @param    	   date            a character vector formated as 'yyyy.mm.dd' (same as the directory date holding the raw data)
#' @param    	   outDirLocation the location of the raw data; the default is getOption('path.to.raw_v2')
#' @return       a data.table
#' @author       MV
#' @examples
#' x = diagnose_pull_v2(date = "2017.03.03")

diagnose_pull_v2 <- function(date, outDirLocation = getOption('path.to.raw_v2'), shiny = FALSE) {
	require(sdb)
	require(data.table)
	require(SNB)
	require(shinytoastr)
  require(doParallel)

	if(shiny) msg = toastr_success else msg = function(x, ...) message(x, ...)
	path = paste0(outDirLocation, paste(year(char2date(date)), date, sep = '/') )

	cl = makePSOCKcluster(detectCores()); registerDoParallel(cl); on.exit( stopCluster(cl) )

	msg( paste('Running diagnostics on raw data (v2) for', date) , progressBar = TRUE, timeOut = 30000)

	ff = list.files(path, full.names = TRUE, recursive = TRUE, pattern = 'BOX\\d{4}.TXT')

 	# run diagnose_raw_txt for all files
		o = foreach( i = 1:length(ff), .packages= c('sdb', 'SNB'), .errorhandling= 'remove')  %dopar% {
			diagnose_raw_txt_v2(ff[i])
		}

		rbindlist(o, fill = TRUE)

  }


