#' @title Pipeline function to compare video and SNB data
#'
#' @description Calls a set of sub-functions to extract data on the match of video data and SNB data of the corresponding dates and times. This function is meant as an easy wrapper for users to assess the advantages and disadvantages of specific parameter settings for the functions that extract SNB events (INs and OUTs).
#'
#' @param con A connection to the database
#' @param timescale The timescale (in seconds) within which the fit with the video data is calculated (only relevant for the last output "Match of SNB and Video data by unit of time").
#' @param ... Any parameters that can be set for the function \link{events_v2()}
#' @return List of length 7 with the following slots:
#' \item{Time difference summary}{Summary of the time differences between Video and SNB data. Note that the time of the videos is here only scored to the minute.}
#' \item{Quality of transponder assignment}{Vector of length 3: \enumerate{\item{Total number of video events without a fitting transponder}\item{Number of video events with a fitting SNB event but without a fitting transponder}\item{Same as before, but excluding SNB direction = "FRONT"} } }
#' \item{Best fit SNB to video}{Dataset where each video event is matched with the SNB event that fits best in time.}
#' \item{Best fit video to SNB}{Dataset where each SNB event is matched with the video event that fits best in time.}
#' \item{SNB data}{SNB events as calculated by the \code{events_v2()} function}
#' \item{Video data}{Video events as scored in the videos. The script that fetches and processes the raw video data can be found in the inst folder of this package.}
#' \item{Match of SNB and Video data by unit of time}{A data.table where each row represents a unit of time as specified by the argument "timescale", and with the following columns:
#'         \itemize{\item{time_} The time in seconds. \item{videoIn} 1 if there was any video activity during the specific unit of time, else 0\item{snbIn} 1 if there was any snb activity during the specific unit of time, else 0\item{snbParentsIn} 1 if there was any snb activity of a parental transponder during the specific unit of time, else 0\item{box} Box number\item{date_} date_\item{time_2}as.POSIXct(time_)\item{timescale}The timescale as specified in the arguments.\item{COL}Colour specification for plot method}}
#'@details The function calls the following functions:\itemize{
#'\item{\link{videoEvaluation_v2}}{ which loads all SNB and Video data, unifies the times, merges the datasets and prepares the first 6 list items of the output.}
#'\item{\link{videoTimeCheck_v2}}{ which calculates from individual "INs" and "OUTs" at which times a birds was at the nestbox}
#'\item{\link{checkIn_v2}}{ which returns the 7th list item of the output}
#'}
#'@author LS
#'@export
#'@examples
#'##not run
#'#create connection
#'con = dbcon('YOUR_USER_NAME')
#'
#'#Set the variables you want to check
#'check = SNBvsVid_v2(con, timescale = 2, max_distance = 30)
#'
#'check[[1]]
#'#--> the max. difference between SNB and video data is less than 1 minute.
#'
#'check[[2]]/nrow(check[[6]])
#'#--> proportion of transponders missed based on definition (see section "Value")
#'
#'#Plot method by unit of time
#'plot(check[[7]], ParentsOnly = FALSE) #All SNB data
#'plot(check[[7]], ParentsOnly = TRUE)  #Only SNB data with the parental transponder
#'#Note that for each box and date the upper row of points represents SNB activity without corresponding video activity, the bottom row of points represents Video activity without corresponding SNB activity and the middle row represents data that match. Activity within 1 unit of time  to activity scored via the other method is still marked green.
#'
#'#table of consistency of SNB vs. video data per unit of time
#'table(check[[7]][, list(videoIn, snbIn)])
#'
#'#table of consistency of SNB vs. video data, if a mismatch by 1 unit of time is "ok"
#'table(check[[7]][,COL]) #"green" = ok, "blue" = event in video, but not SNB data, "red" = event in SNB not video data
#'
#'#table which (detailed) SNB directions are merged with which directions in the video data
#'table(check[[4]][,list(snb_action_detail, vid_action)])
#'
#'#table how detailed SNB directions are translated into non-detailed directions
#'table(check[[4]][,list(snb_action_detail, snb_action)])

SNBvsVid_v2 = function(con, timescale, ...) {
  data("VideoObs", "VideoBoxTimes", "VideoParents")
  a1 = videoEvaluation_v2(...)
  a2 = videoTimeCheck_v2(a1[[6]])
  checkIn = checkIn_v2(a1, timescale = timescale)
  out = a1
  out[[length(out) + 1]] = checkIn
  names(out) = c('Time difference summary', 'Quality of transponder assignment', 'Best fit SNB to video', 'Best fit video to SNB', 'SNB data', 'Video data', 'Match of SNB and Video data by unit of time')
  return(out)
}

#'@title Load and assemble much of the data for video validation
#'@details Called by \link{SNBvsVid_v2}, please follow the link for further details.
videoEvaluation_v2 = function(...) {
    ##### LOAD TIMES #####
    data("VideoObs", "VideoBoxTimes", "VideoParents")
    BoxTimes = copy(as.data.table(VideoBoxTimes))
    Parents = copy(as.data.table(VideoParents))
    VideoData = copy(as.data.table(VideoObs))
    VideoData[, date_ := as.IDate(date_)]

    SNBdata = rbindlist(mapply(FUN = function(x, y, z, con) { dbq(con, paste0("SELECT *, ", x, " as box FROM SNBatWESTERHOLZ_v2.", int2b(x), " where datetime_ > '", y, "' and datetime_ < '", z, "'")) }, x = BoxTimes[, box], y = BoxTimes[, START_sql], z = BoxTimes[, END_sql], MoreArgs = list(con = con), SIMPLIFY = FALSE))
    
    
    SNBdata <- rbindlist(lapply(split(SNBdata, SNBdata[, box]), FUN = function(x) events_v2(x, ...)))
    SNBdata <- merge(SNBdata,Parents,by="box")
    SNBdata[,date_:=as.IDate(in_)]
    SNBdata[,snb_row_id:=1:nrow(SNBdata)]
    setkey(SNBdata,box,in_)

    # ####################################################################################### #
    # ######################## V. MATCH VIDEO AND SNB ACTIONS  ############################## #
    # ####################################################################################### #

    ##### UNIFY TIMES #####
    {	##

      # transponder Peter Loës: 4B76C4B43A6F0001
      # transponder Andrea Wittenzellmer (nest check box 57, 01 June 2017, 10:51): 4CC924B43A6F0001
      # transponder of chick "B4P9740" of box 63: 9640000000008001


      ptr.v <- VideoData[vid_action=="pers_trans",list(box,date_,file_no,datetime_vid,vid_row_id)]
      ptr.v <- ptr.v[,ptr_ste:=ifelse(vid_row_id==min(vid_row_id),1,NA),by=list(box,date_)]
      ptr.v <- ptr.v[,ptr_ste:=ifelse(vid_row_id==max(vid_row_id),2,ptr_ste),by=list(box,date_)]
      ptr.v <- ptr.v[,ptr_ste:=ifelse(is.na(ptr_ste)==TRUE,0,ptr_ste)]
      ptr.v <- ptr.v[box==77 & date_==as.IDate("2017-05-25") & ptr_ste==0,ptr_ste:=3]		# removing rubbish first file for this box-date
      ptr.v <- ptr.v[box==77 & date_==as.IDate("2017-05-25") & ptr_ste==1,ptr_ste:=0]
      ptr.v <- ptr.v[box==77 & date_==as.IDate("2017-05-25") & ptr_ste==3,ptr_ste:=1]


      ptr.s <- SNBdata[transp%in% c("4B76C4B43A6F0001","4CC924B43A6F0001"),list(box,date_,in_,snb_row_id,direction)]
      ptr.s[,ptr_ste:=ifelse(snb_row_id==min(snb_row_id),1,NA),by=list(box,date_)]
      ptr.s[,ptr_ste:=ifelse(snb_row_id==max(snb_row_id),2,ptr_ste),by=list(box,date_)]
      ptr.s[,ptr_ste:=ifelse(is.na(ptr_ste)==TRUE,0,ptr_ste)]
      ptr.s[box==57& date_== as.IDate("2017-05-25") & ptr_ste==0,ptr_ste:=3]		# choosing other reading for this file
      ptr.s[box==57& date_==as.IDate("2017-05-25") & ptr_ste==1,ptr_ste:=0]
      ptr.s[box==57& date_==as.IDate("2017-05-25") & ptr_ste==3,ptr_ste:=1]
      ptr.s[box==77& date_==as.IDate("2017-05-25") & ptr_ste==1, in_:=as.POSIXct("2017-05-25 10:09:59.519",origin="1970-01-01")] # correcting reading of transponder Peter Loës to time of second transponder read




      ptr <- merge(x=ptr.s,y=ptr.v,by=c("box","date_","ptr_ste"),all=TRUE)
      ptr <- ptr[,vs_tdif:=difftime(datetime_vid,in_,units="sec")]
      ptr <- ptr[!is.na(datetime_vid),]
      ptr <- ptr[is.na(vs_tdif),vs_tdif:=0]

      ptr.se  <- copy(ptr)
      ptr.se[,start.v:=min(snb_row_id),by=list(box,date_)]
      ptr.se[,end.v:=max(snb_row_id),by=list(box,date_)]
      ptr.se  <- unique(ptr.se[,list(box,date_,start.v,end.v)])
      ptr.se[is.na(start.v),start.v:=0]
      ptr.se[is.na(end.v),end.v:=10000]

      SNBdata <- merge(SNBdata,ptr.se,by=c("box","date_"))
      SNBdata1 <- SNBdata[start.v==end.v,]
      SNBdata1 <- SNBdata1[snb_row_id>=start.v,]
      SNBdata2 <- SNBdata[start.v!=end.v,]
      SNBdata2 <- SNBdata2[snb_row_id>=start.v & snb_row_id<=end.v,]
      SNBdata <- rbind(SNBdata1,SNBdata2)
      rm(SNBdata1,SNBdata2)
      SNBdata[,start.v:=NULL];SNBdata[,end.v:=NULL]
      setkey(SNBdata,box,date_,in_)


      setnames(VideoData,"datetime_vid","datetime_vid_org")
      VideoData <- merge(VideoData,ptr[,list(box,date_,file_no,vs_tdif)],by=c("box","date_","file_no"),allow.cartesian=TRUE)
      VideoData[,datetime_vid:=as.POSIXct(datetime_vid_org)-vs_tdif]
      VideoData[,vs_tdif:=NULL]
      VideoData[, date_ := as.IDate(date_)]
      TimeCheck <- copy(ptr)

      rm(ptr,ptr.v,ptr.s,ptr.se)

    }	##

    #### PREPARE #####
    {	##

      copy(SNBdata) -> SNBdataOrg #so that this data will be returned
      #split SNB data into ins and outs:available directions: FRONT, IN, OUT
      DIR = list()
      for(i in 1 : nrow(SNBdata)) {
        tmp = copy(SNBdata[i,])
        if(nchar(tmp[1, direction_detail]) == 3) { tmp[, ':=' (datetime_snb = in_, snb_action = direction, snb_action_detail = direction_detail)] } else {
          tmp1 = copy(tmp); tmp1[, datetime_snb := in_]; tmp1[, ':=' (snb_action = strsplit(direction, "-", fixed = TRUE)[[1]][1], snb_action_detail = substring(direction_detail, 1, 3))]
          tmp2 = copy(tmp); tmp2[, datetime_snb := out_]; tmp2[, ':=' (snb_action = strsplit(direction, "-", fixed = TRUE)[[1]][2], snb_action_detail = substring(direction_detail, 5, 7))]
          tmp = rbind(tmp1, tmp2)
        }
        copy(tmp) -> tmpdir

        DIR[[length(DIR)+1]] = copy(tmp)
      }

      SNBdata = rbindlist(DIR)
      SNBdata = SNBdata[, list(box, date_, transp, IDfemale, TRfemale, IDmale, TRmale, snb_row_id, datetime_snb, snb_action, snb_action_detail)]
      SNBdata = unique(SNBdata, by = names(SNBdata)[-which(names(SNBdata) == "snb_row_id")])
      SNBdata[, snb_row_id := 1:nrow(SNBdata)]
      setkey(VideoData,box,datetime_vid)
      setkey(SNBdata,box,datetime_snb)

      # Merge all lines with all lines for each box-day-combination
      vidSNB <- merge(SNBdata,VideoData,by=c("box","date_"),all.x=TRUE,all.y=TRUE,allow.cartesian=TRUE)

      # Time difference between merged events
      vidSNB[,raw_tdif := as.numeric(difftime(datetime_snb,datetime_vid,units="secs"))]#+0.5*duration]


      # consistency in action?
      vidSNB[, same_act := 0]
      vidSNB[snb_action == vid_action, same_act := 1]
      vidSNB[snb_action == "FRONT" & vid_action == "pers_trans", same_act := 1]


      ##### BEST MATCH FOR VIDEO DATA #####
      {	##
        vs <- copy(vidSNB)

        # Show snb id for minimal time difference for each video id
        vs[,best_sfv := snb_row_id[which.min(abs(raw_tdif))], by=vid_row_id]		# best snb ID for video ID

        # Considering best SNB-event for video-event:
        # Check:	Is there a general time difference between corresponding events?
        #			(indicates general time shift)

        #	for(bt in 1:nrow(BoxTimes))	{
        #	print(paste(bt, ": ", median(vs[box==BoxTimes[bt,box] & date_==BoxTimes[bt,date_] & file_no==BoxTimes[bt,file_no] & snb_row_id==best_sfv,raw_tdif],na.rm=TRUE),sep=""))
        #	}

        # add this information
        #vs[,cor_dif:=median(raw_tdif[snb_row_id==best_sfv],na.rm=TRUE),by=list(box,date_,file_no)]

        # remove this time shift:
        #vs[,tdif:=raw_tdif-cor_dif]
        vs[,tdif:=raw_tdif]
       # vs[,cor_dif:=NULL]

        # Again identify snb id for minimal time difference for each video id
        vs[,best_sfv_ := snb_row_id[which.min(abs(tdif))], by=vid_row_id]		# best snb ID for video ID (using adjusted times)

        # Re-check:
        # for(bt in 1:nrow(BoxTimes))	{
        #	print(paste(bt, ": ", median(vs[box==BoxTimes[bt,box] & date_==BoxTimes[bt,date_] & file_no==BoxTimes[bt,file_no] & snb_row_id==best_sfv_,tdif],na.rm=TRUE),sep=""))
        #	}

        vs <- vs[snb_row_id==best_sfv_,]


      }	## (end: best match for video data)

      ##### BEST MATCH FOR SNB DATA #####
      {	##

        sv <- copy(vidSNB)

        # Show video id for minimal time difference for each snb id
        sv[,best_vfs := vid_row_id[which.min(abs(raw_tdif))], by=snb_row_id]		# best video ID for snb ID
        # Cosidering best SNB-event for video-event:
        # Check:	Is there a general time difference between corresponding events?
        #			(indicates general time shift)

        # for(bt in 1:nrow(BoxTimes))	{
        # print(paste(bt, ": ", median(sv[box==BoxTimes[bt,box] & date_==BoxTimes[bt,date_] & file_no==BoxTimes[bt,file_no] & vid_row_id==best_vfs,raw_tdif],na.rm=TRUE),sep=""))
        # }

        # add this information
        #sv[,cor_dif:=median(raw_tdif[vid_row_id==best_vfs],na.rm=TRUE),by=list(box,date_,file_no)]

        # remove this time shift:
        #sv[,tdif:=raw_tdif-cor_dif]
        sv[,tdif:=raw_tdif]
        #sv[,cor_dif:=NULL]

        # Again identify snb id for minimal time difference for each video id
        sv[,best_vfs_ := vid_row_id[which.min(abs(tdif))], by=snb_row_id]		# best video ID for snb ID (using adjusted times)
        # # Re-check:
        # for(bt in 1:nrow(BoxTimes))	{
        # print(paste(bt, ": ", median(sv[box==BoxTimes[bt,box] & date_==BoxTimes[bt,date_] & file_no==BoxTimes[bt,file_no] & vid_row_id==best_vfs_,tdif],na.rm=TRUE),sep=""))
        # }

        sv <- sv[vid_row_id==best_vfs_,]


      }	## (end video data)

      ##### CHECK MUTUAL ASSIGNMENTS #####
      {
        vfors <- sv[,vid_row_id,by=snb_row_id]; setnames(vfors, "vid_row_id","vfs_row_id")
        sforv <- vs[,snb_row_id,by=vid_row_id]; setnames(sforv, "snb_row_id","sfv_row_id")

        vs_ids <- merge(x=vfors,y=sforv,by.x="snb_row_id",by.y="sfv_row_id",all.x=TRUE,all.y=TRUE,allow.cartesian=TRUE)
        consistentIDs <- vs_ids[vfs_row_id==vid_row_id,]

        vs[,same_id:=ifelse(vid_row_id %in% consistentIDs[,vid_row_id],1,0)]
        sv[,same_id:=ifelse(snb_row_id %in% consistentIDs[,snb_row_id],1,0)]

        rm(vfors,sforv)
      }	## (end check mutual assignments)


    } 	## (end: match video and SNB actions)




    {	##

      ##### EVALUATE TIME
      {	##
        # box 077 and box 073:
        # time for personal transponder reading for second video file is correct based on announced time. Time (running seconds) from video analysis is different however. Therefore not corrected above, but only here.
        #LS: Bug fix; these errors are now corrected above.
        #TimeCheck[box=="077" & file_no=="n3",datetime_vid:=as.POSIXct("2017-05-25 12:34:00",origin="1970-01-01")]
        #TimeCheck[box=="077" & file_no=="n3",vs_tdif:=difftime(datetime_vid,datetime_snb,units="sec")]
        #TimeCheck[box=="073" & file_no=="n2",datetime_vid:=as.POSIXct("2017-06-01 12:44:00",origin="1970-01-01")]
        #TimeCheck[box=="073" & file_no=="n2",vs_tdif:=difftime(datetime_vid,datetime_snb,units="sec")]

        TimeCheck = summary(abs(as.numeric(TimeCheck[,vs_tdif])))

      }	## (end: evaluate time)


      ##### EVALUATE TRANSPONDER READINGS
      {	##
        TrCheck = c(
          nrow(vs[is.na(transp) & is.na(vid_comment_txt),] ),
          nrow(vs[is.na(transp) & is.na(vid_comment_txt) & same_act==1,]),
          nrow(vs[is.na(transp) & is.na(vid_comment_txt) & same_act==1 & !snb_action == "FRONT",])
        )

      }	## (end: evaluate transponders)

      #make object
      VideoCheck = list(TimeCheck, TrCheck, vs, sv, SNBdataOrg, VideoData)

    }	## (end: evaluation)
  return(VideoCheck)
}

#'@title Prepare the video data for comparison with SNB data by unit of time.
#'@details Called by \link{SNBvsVid_v2}, please follow the link for further details.
videoTimeCheck_v2 = function(VideoData) {
  copy(VideoData) -> vd
  vd[, datetime_vid := as.POSIXct(as.vector(datetime_vid), origin = "1970-01-01")]
  vd[, vid_action := as.vector(vid_action)]
  IN = which(vd[,vid_action] == "IN")
  OUT = which(vd[,vid_action] == "OUT")

  vd1 = subset(vd, !vid_action %in% c("IN", "OUT"))
  vd1[, in_ := datetime_vid]
  vd1[, out_ := datetime_vid]
  vd1[, vdirection := "FRONT"]

  vd2a = vd[IN, ]
  vd2b = vd[OUT,]
  vd2a[, in_ := datetime_vid]
  vd2a[, out_ := vd2b[,datetime_vid]]
  vd2a[, vdirection := "IN-OUT"]
  vd = data.table(rbind(vd1, vd2a))
  vd[, date_ := as.IDate(as.vector(date_))]
  vd[, out_duration := 0]
  vd[, in_duration := 0]
  setkey(vd, vid_row_id)
  return(vd)
  }


#'@title Helper function for function \link{checkIn_v2}
calcTimeInside_v2 = function(x, timescale){
  x[is.na(out_), ":=" (out_ = in_, out_duration = in_duration)]
  out = mapply(function(x1, x2, x3) { return(round((as.numeric(x1)/timescale), digits = 0) : round((as.numeric(x2 + x3)/timescale), digits = 0)) }, x1 = x[, in_], x2 = x[, out_], x3 = x[, out_duration] )
  out = sort(unique(round(unlist(out), digits = 0)))
}

#'@title Assemble the data for comparing SNB and video data by unit of time.
#'@details Called by \link{SNBvsVid_v2}, please follow the link for further details.
checkIn_v2 = function(vTCout, timescale = 1) {
  checkIn = list()
  data("VideoBoxTimes")
  BoxTimes = copy(as.data.table(VideoBoxTimes))
  for(i in 1 : nrow(BoxTimes)) {
  inside = data.table(time_ = round((as.numeric(as.POSIXct(BoxTimes[i,START])-10)/timescale) : (as.numeric(as.POSIXct(BoxTimes[i,END])+10)/timescale), digits = 0))
  inside[, videoIn := ifelse(time_ %in% calcTimeInside_v2(subset(videoTimeCheck_v2(vTCout[[6]]), box == as.numeric(BoxTimes[i, box]) & date_ == as.IDate(as.character(BoxTimes[i, date_]))), timescale), 1, 0)]
  inside[, snbIn := ifelse(time_ %in% calcTimeInside_v2(subset(vTCout[[5]], box == as.numeric(BoxTimes[i, box]) & date_ == as.IDate(as.character(BoxTimes[i, date_]))), timescale), 1, 0)]
  inside[, snbParentsIn := ifelse(time_ %in% calcTimeInside_v2(subset(vTCout[[5]], (transp == TRfemale | transp == TRmale) & box == as.numeric(BoxTimes[i, box]) & date_ == as.IDate(as.character(BoxTimes[i, date_]))), timescale), 1, 0)]
  inside[, box := BoxTimes[i, box]]
  inside[, date_ := BoxTimes[i, date_]]
  copy(inside) -> checkIn[[i]]
}
checkIn = rbindlist(checkIn)
checkIn[, time_ := time_*timescale]
checkIn[, time_2 := as.POSIXct(time_, origin = "1970-01-01")]
checkIn[, timescale := timescale]

#set colours for plotting
checkIn[, COL := as.character(NA) ]
checkIn[videoIn == 1 & snbIn == 0, COL := 'blue']
checkIn[videoIn == 1 & snbIn == 0 & (shift(snbIn, 1, 0) == 1 | shift(snbIn, 1, 0, type = "lead") == 1), COL := "green", by = list(box, date_)]
checkIn[snbIn == videoIn, COL := "green"]
checkIn[videoIn == 0 & snbIn == 1, COL := 'red']
checkIn[videoIn == 0 & snbIn == 1 & (shift(videoIn, 1, 0) == 1 | shift(videoIn, 1, 0, type = "lead") == 1), COL := "green", by = list(box, date_)]


class(checkIn) = paste( c("SNBcheckIn", class(checkIn)) )

checkIn
}
                                
##Methods definitions

#'@export
setClass("SNBcheckIn",slots = c(x="data.table"))
#'@export
setMethod("plot", signature(x="SNBcheckIn"), function(x, ParentsOnly = TRUE) {
  par(mar = c(5.1, 8.1, 1.1, 1.1))
  data("VideoBoxTimes")
  BoxTimes = copy(as.data.table(VideoBoxTimes))
  PlotBoxTimes = unique(BoxTimes, by = c('box', 'date_'))
  plot(1:2, 1:2, xlim = range(as.ITime(x[,time_2])), ylim = c(0.5,(nrow(PlotBoxTimes)+2)), ylab = '', xlab = "Time", xaxt = 'n', yaxt = 'n')
  axis(2, at = 1:nrow(PlotBoxTimes), labels = paste(PlotBoxTimes[, box], PlotBoxTimes[, date_], sep = ": "), las = 2)
  axis(1, at = (0:23)*60*60, labels = 0:23)
  for(i in 1 : nrow(PlotBoxTimes)) {
    d1 = subset(x, date_ == PlotBoxTimes[i, date_] & box == PlotBoxTimes[i, box])
    d1[, time_2 := as.numeric(as.ITime(time_2))]
    if(ParentsOnly == TRUE) d1[, snbIn := snbParentsIn]

    points(d1[snbIn == 1 & videoIn == 0,time_2], rep(i+0.2, length(d1[snbIn == 1 & videoIn == 0,time_2])), cex = 0.5, col = d1[snbIn == 1 & videoIn == 0, COL], pch = "|")
    points(d1[snbIn == 0 & videoIn == 1,time_2], rep(i-0.2, length(d1[snbIn == 0 & videoIn == 1,time_2])), cex = 0.5, col = d1[snbIn == 0 & videoIn == 1, COL], pch = "|")
    points(d1[snbIn == 1 & videoIn == 1,time_2], rep(i, length(d1[snbIn == 1 & videoIn == 1,time_2])), cex = 0.5, col = d1[snbIn == 1 & videoIn == 1, COL], pch = "|")

  }
  text(10*60*60, nrow(PlotBoxTimes)+0.75, "Activity in video but not SNB (blue if outside one timescale of SNB)", cex = 0.9, adj = 0)
  text(10*60*60, nrow(PlotBoxTimes)+1.25, "Activity in video and SNB", cex = 0.9, adj = 0)
  text(10*60*60, nrow(PlotBoxTimes)+1.75, "Activity in SNB but not video (red if outside one timescale of video)", cex = 0.9, adj = 0)
  
  
  #legend("topright", legend = c("Inside in SNB but not video data", "Inside in SNB and video data", "Inside in video but not SNB data"), col = c("red", "green", "blue"), pch = 16)
})


