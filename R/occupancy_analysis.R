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

interval_occupancy <- function (start_time, end_time, df = ordered_times_df, df_col = "Stay"){
  
  #adds a "Stay" interval column to data frame
  df$Stay <- interval(df$CSPAdmissionTime, df$CSPDischargeTime)
  
  test_interval <- interval(start_time, end_time)
  
  #checks whether any stays overlap with the given time interval 
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  
  #gives the total number or rows that came back as true for overlapping
  sum(overlap_test, na.rm = T)
  
}




#function to find interval occupancy on a given day
#includes any spells, any part of which overlaps with the given day

interval_occupancy_day <- function(date_ymd_in_quotes, df = ordered_times_df, df_col = "Stay"){
  
  #adds a "Stay" interval column to data frame
  df$Stay <- interval(df$CSPAdmissionTime, df$CSPDischargeTime)
  
  #automatically sets start and end times to beginning and end of the day
  start_time <- paste(date_ymd_in_quotes, "00:00:00 BST", sep = " ")
  end_time <- paste(date_ymd_in_quotes, "23:59:59 BST", sep = " ")
  
  #as before
  test_interval <- interval(start_time, end_time)
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  sum(overlap_test, na.rm = T)
  
}




#function to find instantaneous hospital occupancy at a given time

hospital_occupancy <- function(date_and_time, df = ordered_times_df, time_in_col = "CSPAdmissionTime", time_out_col = "CSPDischargeTime"){
  
  #adds a "Stay" interval column to data frame
  df$Stay <- interval(df[,time_in_col], df[,time_out_col]) 
  
  test_interval <- interval(date_and_time, date_and_time)
  overlap_test <- int_overlaps(df$Stay, test_interval)
  sum(overlap_test, na.rm = T)
  
}




#function to plot hourly occupancy in a given time interval
# put the data frame you want to plot from into plot_df argument
plot_hospital_occupancy <- function(start_date_time, end_date_time, plot_df = ordered_times_df){
  
  #creates a data frame to help with ggplot
  time <- seq(as.POSIXlt(start_date_time), as.POSIXlt(end_date_time), by="hours")
  hourly_occupancy <- sapply(time, hospital_occupancy, df = plot_df) 
  occupancy_df <- data.frame(time, hourly_occupancy)
  
  colours <- ifelse(hourly_occupancy >= 450, "red", "blue")  ###arbitrary max value - needs to be changed
  
  ggplot(occupancy_df, aes(x=time, y=hourly_occupancy)) + geom_line() +geom_point(col = colours) + xlab("Time") + 
    ylab("Hospitial Occupancy") + ggtitle("Hourly Hospital Occupancy") 
  
}



