#needs to be sorted out so that it can be applied to any df


#function to calculate the hospital occupancy each hour and store in a df
#and plot
hosp_occ <- function(start_date_time, end_date_time, df = ordered_times_df, time_in_col = "CSPAdmissionTime", time_out_col = "CSPDischargeTime", episode_col = "EpisodeNumber"){
  
  
  #reduces data frame to only have one row per spell for each patient to avoid double counting. 
  #skips doing this if there is no episode column in the data frame
  #can't get get_spells to work here...
  if(episode_col %in% colnames(df)){
    df <- df[which(df[,episode_col] == 1),]
  }
  
  #sets up vector of time by hour
  time <- seq(as.POSIXlt(start_date_time), as.POSIXlt(end_date_time), by="hours")
  
  #fills df with 1s in the right places and sums rows
  no_of_patients <- 1:nrow(df)
  occ_df <- data.frame(sapply(no_of_patients, get_personal_occ_vect, time_vect = time, df1 = df, time_in_col1 = time_in_col, time_out_col1 = time_out_col))
  #colnames(occ_df)[1:5] <- 1:5
  occ_df$occupancy_vect <- rowSums(occ_df)
  occ_df$Time <- time
  
  occ_df$occupancy_vect
  
  
  #plotting
  #variables to set pip spacing on x axis depending on length of time used
  # no_of_pips <- length(time) %/% 12
  # no_of_pips_string <- paste(no_of_pips, "hour")
  # 
  # 
  # ggplot(occ_df, aes(x= Time, y = occupancy_vect)) + geom_line( size = 1)  +
  #   xlab("Time") + ylab("Hospital Occupancy") + ggtitle("Hourly Hospital Occupancies") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #   theme(axis.text.x=element_text(angle=35, vjust=0.5)) + scale_x_datetime(date_breaks = no_of_pips_string) + 
  #   scale_y_continuous(breaks = (pretty_breaks()))
  
}




#function to get the vector of 1s and 0s corresponding to the hours that the patient is in the hospital
get_personal_occ_vect <- function(row_no, time_vect, df1, time_in_col1, time_out_col1){
  
  #sets up zeros vector of same length as time vector
  personal_occ_vect <- integer(length(time_vect))
  
  #finds index of personal_occ_vect to put first 1 into 
  index1 <- interval(time_vect[1], df1[row_no,time_in_col1]) %/% dhours() + 1
  #print(index1)
  #finds number of 1s to put after first index
  no_of_ones <- interval(df1[row_no, time_in_col1],df1[row_no, time_out_col1]) %/% dhours()
  index2 <- index1 + no_of_ones 
  
  #conditions for spells that start and/or end before and/or after the specified interval
  if(index1 < 1 & (!is.na(index1))){                       #for spells that start before the interval
    index1 <- 1
  }
  if(index2 > length(time_vect) & (!is.na(index2))){       #for spells that finish before the interval
    index2 <- length(time_vect)
  }
  
  if((index1 > length(time_vect) | index2 < 1) & (!is.na(index1))){ #for spells that don't overlap at all
    personal_occ_vect <- 0
  } else if((!is.na(index1)) | (!is.na(index2))){
    personal_occ_vect[index1 : index2] <- 1
  }
  
  personal_occ_vect
  #print(personal_occ_vect)
  # print(length(personal_occ_vect))
  
}

