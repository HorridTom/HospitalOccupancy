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
#includes any spells any part of which overlaps the specified interval

interval_occupancy <- function (start_time, end_time, df = ordered_times_df, df_col = "Stay"){
  test_interval <- interval(start_time, end_time)
  
  #checks whether any stays overlap with the given time interval 
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  
  #gives the total number or rows that came back as true for overlapping
  sum(overlap_test, na.rm = T)
  
}




#function to find occupancy on a given day
occupancy_day <- function(date_ymd_in_quotes, df = ordered_times_df, df_col = "Stay"){
  #automatically sets start and end times to beginning and end of the day
  start_time <- paste(date_ymd_in_quotes, "00:00:00 BST", sep = " ")
  end_time <- paste(date_ymd_in_quotes, "23:59:59 BST", sep = " ")
  
  #as before
  test_interval <- interval(start_time, end_time)
  overlap_test <- int_overlaps(df[,df_col], test_interval)
  sum(overlap_test, na.rm = T)
}