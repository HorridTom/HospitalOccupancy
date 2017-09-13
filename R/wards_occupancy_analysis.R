#function to calculate the occupancy of a given ward at a given time


ward_occupancy <- function(date_and_time, ward, df = ward_test_reduced_df, time_in_col = "EpisodeStartDate", time_out_col = "EpisodeEndDate", ward_col = "CSPLastWard"){
  
  #checks to see whether the ward entered by the user is a valid ward, if so calculates occupancy
  ward_check <- ward == df[,ward_col]
  
  if( (all(!ward_check, na.rm = TRUE)) == TRUE){
    
  writeLines("The name of the ward you have entered is not on the system. \nPlease check that your ward abbreviation is correct.")
    
  } else {
    
    #adds a column to df for "Stay" interval
    df$Stay <- interval(df[,time_in_col], df[,time_out_col])
    
    test_interval <- interval(date_and_time, date_and_time)
    ward_specific_df <- df[which(df[,ward_col] == ward),]
    overlap_test <- int_overlaps(ward_specific_df$Stay, test_interval)
    sum(overlap_test, na.rm = T)
    
  }
  
}




#function to plot hourly ward occupancy in a given time interval
# put the data frame you want to refer to into plot_df argument
# enter wards to plot in wards vector argument

plot_ward_occupancies <- function(start_date_time, end_date_time, wards_as_vect, plot_df = ward_test_reduced_df){
  
  
    #creates a data frame to help with ggplot
    time <- seq(as.POSIXlt(start_date_time), as.POSIXlt(end_date_time), by="hours")
    df_to_plot <- data.frame(Time = time)
    
    #for loop to add a column to the data frame to be plotted (each corresponding to occupancy of each ward)
    for(i in 1:length(wards_as_vect)){
      
      hourly_ward_occupancy <- sapply(time, ward_occupancy, ward = wards_as_vect[i], df = plot_df)
      ward_name <- wards_as_vect[i]
      df_to_plot <- data.frame(df_to_plot, hourly_ward_occupancy)
      colnames(df_to_plot)[i+1] <- ward_name
      
    }
    
    #print(df_to_plot)
    
    #uses melt function from reshape2 library to allow us to plot the whole data frame 
    df_to_plot <- melt(df_to_plot, id.vars = 'Time', variable.name = 'Ward')
    ggplot(df_to_plot, aes(x= df_to_plot[,1], y = value)) + geom_line(aes(colour = Ward)) +geom_point(aes(colour = Ward)) + xlab("Time") + ylab("Ward Occupancy") + ggtitle("Hourly Ward Occupancies") 
}