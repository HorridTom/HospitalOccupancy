#function to reduce data frame to only first spells

get_spells <- function(df, episodeCol = "EpisodeNumber"){
  
  df_reduced <- df[which(df[,episodeCol] == 1),]
  df_reduced
  
}


#function to take a sample of the spells data

get_spells_sample <- function(spells_df, number_of_patients, ID_col_name = "PseudoID"){
  
  patients <- unique(spells_df[,ID_col_name])
  patients_sample <- head(patients, number_of_patients)
  spells_sample <- spells_df[which(spells_df[,ID_col_name] %in% patients_sample),]
  spells_sample
  
}





#function to find occupancy between a certain time interval
#includes any spells, any part of which overlaps the specified interval
#require(lubridate)

interval_occupancy <- function (start_time, end_time, df = ordered_times_df, df_col = "Stay"){
  
  df$Stay <- interval(df$CSPAdmissionTime, df$CSPDischargeTime)
  test_interval <- interval(start_time, end_time)
  
  #checks whether any stays overlap with the given time interval 
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  sum(overlap_test, na.rm = T)
  
}




#function to find interval occupancy on a given day
#includes any spells, any part of which overlaps with the given day
#require(lubridate)

interval_occupancy_day <- function(date_ymd_in_quotes, df = ordered_times_df, df_col = "Stay"){
  

  df$Stay <- interval(df$CSPAdmissionTime, df$CSPDischargeTime)
  start_time <- paste(date_ymd_in_quotes, "00:00:00 BST", sep = " ")
  end_time <- paste(date_ymd_in_quotes, "23:59:59 BST", sep = " ")

  test_interval <- interval(start_time, end_time)
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  sum(overlap_test, na.rm = T)
  
}




#function to find instantaneous hospital occupancy at a given time
#require(lubridate)

hospital_occupancy <- function(date_and_time, df = ordered_times_df, time_in_col = "CSPAdmissionTime", time_out_col = "CSPDischargeTime", episode_col = "EpisodeNumber"){
  
  #reduces data frame to only have one row per spell for each patient to avoid double counting. 
  if(episode_col %in% colnames(df)){
  df <- df[which(df[,episode_col] == 1),]
  }
  
  df$Stay <- interval(df[,time_in_col], df[,time_out_col]) 
  
  test_interval <- interval(date_and_time, date_and_time)
  overlap_test <- int_overlaps(df$Stay, test_interval)
  sum(overlap_test, na.rm = T)
  
}




#function to plot hourly occupancy in a given time interval
# put the data frame you want to plot from into plot_df argument
#require(lubridate), require(ggplot), require(scales)

plot_hospital_occupancy <- function(start_date_time, end_date_time, plot_df = ordered_times_df, time_in_col = "CSPAdmissionTime", time_out_col = "CSPDischargeTime", episode_col = "EpisodeNumber"){

  
  #creates a data frame to help with ggplot
  time <- seq(as.POSIXlt(start_date_time), as.POSIXlt(end_date_time), by="hours")
  hourly_occupancy <- sapply(time, hospital_occupancy, df = plot_df, time_in_col, time_out_col, episode_col) 
  occupancy_df <- data.frame(time, hourly_occupancy)
  
  #colours points that are over the threshold to red
  colours <- ifelse(hourly_occupancy >= 450, "red", "blue")  ###arbitrary max value - needs to be changed
  
  if(length(time)%/%12 == 0){
    pip_spacing <- 1
  } else {
    pip_spacing <- length(time) %/% 12
  }
  
  pip_spacing_string <- paste(pip_spacing, "hour")
  
  ggplot(occupancy_df, aes(x=time, y=hourly_occupancy)) + geom_line() + geom_point(col = colours) + 
    xlab("Time") + ylab("Hospitial Occupancy") + ggtitle("Hourly Hospital Occupancy") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.text.x=element_text(angle=35, vjust=0.5)) + 
    scale_x_datetime(date_breaks = pip_spacing_string) + 
    scale_y_continuous(breaks = (pretty_breaks()))
}



