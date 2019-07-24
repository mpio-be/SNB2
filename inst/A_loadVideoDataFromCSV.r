
# ####################################################################################### #
# ################################ I. PRELIMINARIES ##################################### #
# ####################################################################################### #

# run every time

rm(list=ls())		# clear workspace

{	##

  ######### LOADING PACKAGES ################################################################


  require(data.table)
  require(SNB2)
  require(sdb)
  require(stringr)

  ######### SETTING CREDENTIALS, PATHS, FILE NAMES ETC. #####################################

  # choose credentials:
  usern <- "lschlicht"
  # choose paths / file names / data set:
  path.data <- "/ds/grpkempenaers/Lotte/R Studio projects/Data for package SNBevents/Emmis data/2017NewSNBCheck"

  ######### SETTING CONNECTION ##############################################################

  con = dbcon(user = usern)


}	## (end preliminaries)




# ####################################################################################### #
# ################################ II. FETCH DATA ####################################### #
# ####################################################################################### #

{	###

  ##### GENERAL DATA #####
  {	##

    BoxTimes <- data.table(read.table(paste(path.data,"/VideoData/BoxTimes.txt",sep=""),header=TRUE))
    BoxTimes[,box_no:=str_pad(box_no, 3, side = "left", pad = "0")]
    BoxTimes[,START_sql:=as.character(as.POSIXct(START)-600)]	# add 10 minutes before for extracting corresponding SNB data
    BoxTimes[,END_sql:=as.character(as.POSIXct(END)+600)]		# add 10 minutes after
    BoxTimes[is.na(END) & box_no=="063" & date_string=="20170601",END_sql:="2017-06-01 11:51:00"]			# define end of time interval for box where end is missing
    BoxTimes[,sql := paste0("SELECT UNIX_TIMESTAMP(datetime_) as datetime_, sensor_value, sensor, r_pk, path  FROM SNBatWESTERHOLZ_v3.b",box_no,' WHERE datetime_ BETWEEN ', shQuote(START_sql), ' AND ', shQuote(END_sql))]		# write SQL-statement for extraction of SNB-data
    #add information about the length of the first video(s); adding video length to BoxTimes
    BoxTimes[box_no == "006", n1_length := as.numeric(as.ITime("1:31:18"))]
    BoxTimes[box_no == "034", n1_length := as.numeric(as.ITime("1:31:13"))]
    BoxTimes[box_no == "057", n1_length := as.numeric(as.ITime("1:32:35"))]
    BoxTimes[box_no == "063", n1_length := as.numeric(as.ITime("1:29:58"))]
    BoxTimes[box_no == "064", n1_length := as.numeric(as.ITime("1:31:10"))]
    BoxTimes[box_no == "073", n1_length := as.numeric(as.ITime("1:31:11"))]
    BoxTimes[box_no == "076", n1_length := as.numeric(as.ITime("1:31:13"))]
    BoxTimes[box_no == "077", n1_length := as.numeric(as.ITime("00:00:27"))]
    BoxTimes[box_no == "077", n2_length := as.numeric(as.ITime("1:31:10"))]


    Parents <- data.table(box_no=c("034","006","057","063","064","073","076","077"),
                          IDfemale=c("B4P8157","B4P8159","B4P9807","B4P9823","B4H0805","B4P8186","B4H0389","B2X4920"),
                          TRfemale=c("7BC213A801AF0001","8AC213A801AF0001","8980000000008001","B440000000008001","4C2213A801AF0001","A16406ACBA100001","A144AF0213800001","7AAAC4B43A6F0001"),
                          IDmale=c("B4H0184","B4P8354","B4H0502","B4H0958","B4P8178","B4P8213","B4P8196","B4H1045"),
                          TRmale=c("EA2213A801AF0001","AA8213A801AF0001","608213A801AF0001","D78213A801AF0001","9AC213A801AF0001","712213A801AF0001","6500000000008001","FFFE96A801AF0001"))

  }	## (end: fetch general data)

  ##### VIDEO DATA #####
  {	##

    if(exists("VideoData")==TRUE)	rm(VideoData)
    for (i in 1:nrow(BoxTimes))	{
      vid.dat <- data.table(read.csv(paste(path.data,"/VideoData/",BoxTimes[i,date_string],"_Box",BoxTimes[i,box_no],"_",BoxTimes[i,file_no],".csv",sep=""),header=TRUE))	# fetch video data
      setnames(vid.dat,c("time","code","class","comment"),c("time_s","code_no","class_no","comment_txt"))	# rename

      #correct raw data errors in video data (IN or OUT is accidentally noted twice, FRONT should be IN)
      if(BoxTimes[i,box_no] == "064" & BoxTimes[i, file_no] == "n2") {vid.dat = vid.dat[-which(vid.dat[,time_s == "7,336,878"]),]}
      if(BoxTimes[i,box_no] == "064" & BoxTimes[i, file_no] == "n1") {vid.dat = vid.dat[-which(vid.dat[,time_s == "4,144,158"]),]}
      if(BoxTimes[i,box_no] == "057" & BoxTimes[i, file_no] == "n2") {vid.dat[time_s == "3,141,293", code_no := "2"]}
      if(BoxTimes[i,box_no] == "064" & BoxTimes[i, file_no] == "n2") {vid.dat[time_s == "7,335,318", code_no := "2"]}

      vid.dat[,box_no :=BoxTimes[i,box_no]]				# add info
      vid.dat[,start_time := BoxTimes[i,START]]
      vid.dat[,end_time := BoxTimes[i,END]]
      vid.dat[,file_no := BoxTimes[i,file_no]]
      vid.dat[,date_string := BoxTimes[i,date_string]]
      vid.dat[,n1_length := BoxTimes[i,n1_length]]
      vid.dat[,n2_length := BoxTimes[i,n2_length]]

      vid.dat[,time_s:=as.character(as.vector(time_s))]		# reformat
      vid.dat[,code_no:=as.character(as.vector(code_no))]
      vid.dat[,class_no:=as.character(as.vector(class_no))]
      vid.dat[,comment_txt:=as.character(as.vector(comment_txt))]
      vid.dat[,start_time:=as.POSIXct(as.vector(start_time),origin = "1970-01-01")]
      vid.dat[,end_time:=as.POSIXct(as.vector(end_time),origin = "1970-01-01")]

      # deal with multiple decimal/thousand markers as points and commata
      vid.dat[,time_dec_com:=str_count(time_s,",")]
      vid.dat[,time_dec_dot:=str_count(time_s,"\\.")]

      vid.dat[time_dec_com>0, dec_pos := rbindlist(lapply(str_locate_all(vid.dat[time_dec_com>0,time_s], ","),function(x)	{x <- data.table(x)[nrow(x),]; return(x)}))[,start]]
      vid.dat[time_dec_com>0,time_s_nondec:=str_sub(time_s,1,dec_pos-1)]
      vid.dat[time_dec_com>0,time_s_nondec:=str_replace(time_s_nondec,",","")]
      vid.dat[time_dec_com>0,time_s_dec:=str_sub(time_s,dec_pos+1)]
      vid.dat[time_dec_dot>0, dec_pos := rbindlist(lapply(str_locate_all(vid.dat[time_dec_dot>0,time_s], "\\."),function(x)	{x <- data.table(x)[nrow(x),]; return(x)}))[,start]]
      vid.dat[time_dec_dot>0,time_s_nondec:=str_sub(time_s,1,dec_pos-1)]
      vid.dat[time_dec_dot>0,time_s_nondec:=str_replace(time_s_nondec,"\\.","")]
      vid.dat[time_dec_dot>0,time_s_dec:=str_sub(time_s,dec_pos+1)]

      vid.dat[,time_s:=paste(time_s_nondec,time_s_dec,sep=".")]

      vid.dat[,c("time_dec_com","time_dec_dot","dec_pos","time_s_dec","time_s_nondec"):=NULL]
      vid.dat[,time_s:=as.numeric(as.vector(time_s))]

      if(exists("VideoData")==TRUE)	{VideoData <- rbind(VideoData,vid.dat)}		# append in one file
      if(exists("VideoData")==FALSE)	{VideoData <- vid.dat}
      rm(vid.dat)
    }
    rm(i)
    setkey(VideoData,date_string,box_no,file_no,time_s)
    VideoData[,vid_row_id:=1:nrow(VideoData)]

  }	## (end: fetch video data)





}	##	(end: fetch data)




# ####################################################################################### #
# ########################### III. REFORMAT VIDEO DATA ################################## #
# ####################################################################################### #

{	##

  ##### SUMMARIZE EVENTS BELONGING TOGETHER #####
  # based on going through code sequences occurring in data set (see written notes)
 #REMOVED HERE!



  ##### RECODING CLASS AND CODE #####
  {	##
    # code_no:
    #   1: start personal transponder
    #   2: bird enters with no partner inside
    #   3: bird exits with no partner inside
    #   4: bird enters with partner inside
    #   5: bird exits with partner inside
    #   6: bird lands at hole outside
    #   7: bird appears at hole inside
    #   8: bird departs from hole outside with no entry
    #   9: bird disappears from hole inside with no exit
    #  10: unclear event
    # END: end of recording
    # class_no:
    # as code, except:
    # class_no = 0 for code_no = END


    ## three new columns:
    VideoData[,vid_action:=as.character(NA)]
    VideoData[,partner_inside:=as.numeric(NA)]
    VideoData[,actions_start_end:=as.numeric(NA)]
    # peform recoding:
    VideoData[code_no=="END",vid_action:="end_rec"]
    VideoData[code_no=="END",partner_inside:=NA]
    VideoData[code_no=="END",actions_start_end:=NA]
    VideoData[code_no==1,vid_action:="pers_trans"]
    VideoData[code_no==1,partner_inside:=NA]
    VideoData[code_no==1,actions_start_end:=0]
    VideoData[code_no==2,vid_action:="IN"]
    VideoData[code_no==2,partner_inside:=0]
    VideoData[code_no==2,actions_start_end:=0]
    VideoData[code_no==3,vid_action:="OUT"]
    VideoData[code_no==3,partner_inside:=0]
    VideoData[code_no==3,actions_start_end:=0]
    VideoData[code_no==4,vid_action:="IN"]
    VideoData[code_no==4,partner_inside:=1]
    VideoData[code_no==4,actions_start_end:=0]
    VideoData[code_no==5,vid_action:="OUT"]
    VideoData[code_no==5,partner_inside:=1]
    VideoData[code_no==5,actions_start_end:=0]
    VideoData[code_no==6,vid_action:="FRONT"]
    VideoData[code_no==6,partner_inside:=NA]
    VideoData[code_no==6,actions_start_end:=1]
    VideoData[code_no==7,vid_action:="BACK"]
    VideoData[code_no==7,partner_inside:=NA]
    VideoData[code_no==7,actions_start_end:=1]
    VideoData[code_no==8,vid_action:="FRONT"]
    VideoData[code_no==8,partner_inside:=NA]
    VideoData[code_no==8,actions_start_end:=2]
    VideoData[code_no==9,vid_action:="BACK"]
    VideoData[code_no==9,partner_inside:=NA]
    VideoData[code_no==9,actions_start_end:=2]
    VideoData[code_no==10,vid_action:="NA"]
    VideoData[code_no==10,partner_inside:=NA]
    VideoData[code_no==10,actions_start_end:=NA]
    # delete code and class columns:
    VideoData[,c("code_no","class_no") := NULL]

    # adjust FRONT to IN for cases sorted out above
    #VideoData[front_to_in==1,vid_action:="IN"]
    #VideoData[,front_to_in:=NULL]

    # remove end rows
    VideoData <- VideoData[vid_action!="end_rec",]

  }	## (end: recode class and code)


  ##### REDEFINE TIME #####
  {	##

    setnames(VideoData, "time_s","time_s_file")

    VideoData[file_no=="n1",time_s_start1:=min(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[file_no=="n1",time_s_end1:=max(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[,time_s_start1:=unique(time_s_start1[!is.na(time_s_start1)]),by=list(box_no,date_string)]
    VideoData[,time_s_end1:=unique(time_s_end1[!is.na(time_s_end1)]),by=list(box_no,date_string)]
    VideoData[file_no=="n2",time_s_start2:=min(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[file_no=="n2",time_s_end2:=max(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[,time_s_start2:=unique(time_s_start2[!is.na(time_s_start2)]),by=list(box_no,date_string)]
    VideoData[,time_s_end2:=unique(time_s_end2[!is.na(time_s_end2)]),by=list(box_no,date_string)]
    VideoData[file_no=="n3",time_s_start3:=min(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[file_no=="n3",time_s_end3:=max(time_s_file,na.rm=TRUE),by=list(box_no,date_string)]
    VideoData[,time_s_start3:=unique(time_s_start3[!is.na(time_s_start3)]),by=list(box_no,date_string)]
    VideoData[,time_s_end3:=unique(time_s_end3[!is.na(time_s_end3)]),by=list(box_no,date_string)]

    VideoData[file_no=="n1",time_s:=time_s_file]
    VideoData[file_no=="n2" & time_s_start2>time_s_end1,time_s:=time_s_file]
    VideoData[file_no=="n2" & time_s_start2<time_s_end1,time_s:=time_s_file+n1_length]
    VideoData[file_no=="n3" & time_s_start3>time_s_end2,time_s:=time_s_file]
    VideoData[file_no=="n3" & time_s_start3<time_s_end2,time_s:=time_s_file+n1_length+n2_length]

    VideoData[,datetime_vid:=start_time+time_s]
    VideoData[,video_dur:=difftime(end_time,start_time,units="secs")]
    VideoData[,c("time_s_start1","time_s_end1","time_s_start2","time_s_end2","time_s_start3","time_s_end3"):=NULL]

  }	## (end: redefine time)

  VideoData[,vid_row_id:=1:nrow(VideoData)]
  VideoData[, ':=' (n1_length = NULL, n2_length = NULL)]
  #setnames(VideoData,c("event_id","time_s","time_s_file","between","partner_inside","actions_start_end","start_time","end_time","comment_txt","video_dur"),c("vid_event_id","vid_time_s","vid_time_s_file","vid_between","vid_part_ins","vid_act_se","vid_start_time","vid_end_time","vid_comment_txt","vid_dur"))
  setnames(VideoData,c("time_s","time_s_file","partner_inside","actions_start_end","start_time","end_time","comment_txt","video_dur"),c("vid_time_s","vid_time_s_file","vid_part_ins","vid_act_se","vid_start_time","vid_end_time","vid_comment_txt","vid_dur"))


}	## (end: reformat video data)

{ #remove unnecessary data from VideoData #####
  setnames(VideoData, "box_no", 'box')
  VideoData[, date_ := as.IDate(vid_start_time)]
  VideoData = VideoData[, list(box, file_no, vid_row_id, date_, vid_action, datetime_vid, vid_dur, vid_comment_txt, vid_act_se)]
  }

#add one entry to video data where the bird was inside the nestbox when the video was started (to allow proper comparison!)
VideoData = rbind(VideoData, data.table(box = "034", file_no = "n1", vid_row_id = NA, date_ = as.IDate("2017-05-18"), vid_action = "IN", datetime_vid = as.POSIXct("2017-05-18 09:53:26.000"), vid_dur = difftime(as.POSIXct(7980, origin = "1970-01-01"), as.POSIXct(0, origin = "1970-01-01"), units = 'secs'), vid_comment_txt = NA, vid_act_se = 0))

#adjust comments that are problematic for opening the .csv
VideoData[box == "057" & file_no == "n1" & datetime_vid == "2017-06-01 10:22:25.278", vid_comment_txt := "Peter Loes Transponder 10:22"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-05-25 12:08:06.973", vid_comment_txt := "BT2 and BT1 both at front plate from oppsite sides"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-05-25 12:08:07.000", vid_comment_txt := "BT2 and BT1 both at front plate from oppsite sides"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-05-25 12:08:10.400", vid_comment_txt := "BT1 back in box"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-05-25 12:08:19.571", vid_comment_txt := "BT1 leaves box"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-06-01 12:11:55.882", vid_comment_txt := "BT in nest opening for 17 secs"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-06-01 12:38:12.552", vid_comment_txt := "BT in nest opening for 5 secs"]
VideoData[box == "057" & file_no == "n2" & datetime_vid == "2017-06-01 12:27:45.557", vid_comment_txt := "BT in nest opening for 9 secs"]
VideoData[box == "064" & file_no == "n1" & datetime_vid == "2017-05-18 10:50:33.673", vid_comment_txt := "BT 1 BACK"]
VideoData[box == "064" & file_no == "n1" & datetime_vid == "2017-05-18 10:51:32.500", vid_comment_txt := "BT 2 off as it sees BT 1"]
VideoData[box == "064" & file_no == "n1" & datetime_vid == "2017-05-18 10:51:35.588", vid_comment_txt := "BT 1 OUT"]
VideoData[box == "076" & file_no == "n1" & datetime_vid == "2017-05-25 10:11:27.939", vid_comment_txt := "BT can't be seen, probably inside looking out"]
VideoData[box == "076" & file_no == "n1" & datetime_vid == "2017-05-25 10:11:31.538", vid_comment_txt := "BT can't be seen, probably inside looking out"]
VideoData[box == "076" & file_no == "n1" & datetime_vid == "2017-05-25 10:11:34.301", vid_comment_txt := "BT can't be seen, probably inside looking out"]
VideoData[box == "076" & file_no == "n1" & datetime_vid == "2017-05-25 10:11:39.339", vid_comment_txt := "BT can't be seen, probably inside looking out"]
VideoData[box == "076" & file_no == "n1" & datetime_vid == "2017-05-25 10:11:41.739", vid_comment_txt := "BT can't be seen, probably inside looking out"]
VideoData[box == "076" & file_no == "n2" & datetime_vid == "2017-05-25 11:47:55.171", vid_comment_txt := "BT 1 leaves"]


setkey(VideoData, box, datetime_vid)
VideoData[, vid_row_id := 1:nrow(VideoData)]

setnames(Parents, "box_no", 'box')
setnames(BoxTimes, "box_no", 'box')
setnames(BoxTimes, "date_string", 'date_')
BoxTimes[, date_ := as.IDate(as.character(date_), format = "%Y%m%d")]

VideoData = subset(VideoData,
                  (box != "057" | date_ != "2017-06-01") &
                    (box != "073" | date_ != "2017-06-01") &
                    (box != "076" | date_ != "2017-05-25")) #boxes with disturbance of inner light barrier/technical issue removed for the remaining boxes
BoxTimes = subset(BoxTimes,
  (box != "057" | date_ != "2017-06-01") &
  (box != "073" | date_ != "2017-06-01") &
  (box != "076" | date_ != "2017-05-25")) #boxes with disturbance of inner light barrier/technical issue removed for the remaining boxes
#repair missing END for box 63
BoxTimes[box == "063", END := "2017-06-01 11:51:00"]


# save #####
write.table(Parents, file = "/ds/grpkempenaers/Lotte/R Studio projects/SNBevents/data/VideoParents.csv", sep = ";")
write.table(VideoData, file = "/ds/grpkempenaers/Lotte/R Studio projects/SNBevents/data/VideoObs.csv", sep = ";")
write.table(BoxTimes, file = "/ds/grpkempenaers/Lotte/R Studio projects/SNBevents/data/VideoBoxTimes.csv", sep = ";")

