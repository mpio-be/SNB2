
##Version 2: SNB-Heaven
#1. copy-paste all functions to R
#2. run function "fetch_data_v2" (for input information see help file of function fetch_data in SNB package)
#3. run function "events_v2"
#4. author: LS
#5. not uploading function available yet

events_v2 = function(x) {
    x = fetch_ins_outs_v2(x, threshold)
    if (nrow(x) == 0) 
      return()
    x = assign_direction_v2(x)
    if (nrow(x) == 0) 
      return()
    x = concat_events_v2(x)
    if(nrow(x) == 0) 
      return()
    x = combine_front_v2(x)
    return(x)
}