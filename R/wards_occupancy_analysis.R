#function to calculate the occupancy of a given ward at a given time
#require(lubridate)


ward_occupancy <- function(date_and_time, ward, df = ward_test_reduced_df, time_in_col = "EpisodeStartDate", time_out_col = "EpisodeEndDate", ward_col = "CSPLastWard"){
  
  #checks to see whether the ward entered by the user is a valid ward, if so calculates occupancy
  ward_check <- ward == df[,ward_col]
  
  if( (all(!ward_check, na.rm = TRUE)) == TRUE){
    
  writeLines("The name of the ward you have entered is not on the system. \nPlease check that your ward abbreviation is correct.")
    
  } else {
    
    #adds a column to df for "Stay" interval
    df$Stay <- interval(df[,time_in_col], df[,time_out_col])
    
    #checks how many stays overlap with the given time 
    test_interval <- interval(date_and_time, date_and_time)
    ward_specific_df <- df[which(df[,ward_col] == ward),]
    overlap_test <- int_overlaps(ward_specific_df$Stay, test_interval)
    sum(overlap_test, na.rm = T)
    
  }
  
}




#function to plot hourly ward occupancy in a given time interval
#put the data frame you want to refer to into plot_df argument
#enter wards to plot in wards vector argument e.g. wards_as_vect = c("RRU","JAME")
#require(lubridate), require(ggplot2), require(reshape2), require(scales)
#good test line: plot_ward_occupancies("2012-01-01 00:00:00","2012-01-14 00:00:00", c("RRU","SALM","JAME", "JRCC"))
plot_ward_occupancies <- function(start_date_time, end_date_time, wards_as_vect, plot_df = ward_test_reduced_df){
  
  
    #creates a data frame to help with ggplot 
    time <- seq(as.POSIXlt(start_date_time), as.POSIXlt(end_date_time), by= "hours")
    df_to_plot <- data.frame(Time = time)
    
    #for loop to add a column to the data frame to be plotted (each corresponding to occupancy of each ward)
    for(i in 1:length(wards_as_vect)){
      
      hourly_ward_occupancy <- sapply(time, ward_occupancy, ward = wards_as_vect[i], df = plot_df)
      df_to_plot <- data.frame(df_to_plot, hourly_ward_occupancy)
      ward_name <- wards_as_vect[i]
      colnames(df_to_plot)[i+1] <- ward_name
      
    }
    
    #print(df_to_plot)
    #variables to set pip spacing on x axis depending on length of time 
    if(length(time)%/%12 == 0){
      pip_spacing <- 1
    } else {
      pip_spacing <- length(time) %/% 12
    }
    
    no_of_pips_string <- paste(pip_spacing, "hour")
 
    
    #uses melt function from reshape2 library to allow us to plot the whole data frame 
    df_to_plot <- melt(df_to_plot, id.vars = 'Time', variable.name = 'Ward')
    ggplot(df_to_plot, aes(x= df_to_plot[,1], y = value)) + geom_line(aes(colour = Ward), size = 1)  +
      xlab("Time") + ylab("Ward Occupancy") + ggtitle("Hourly Ward Occupancies") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        theme(axis.text.x=element_text(angle=35, vjust=0.5)) + scale_x_datetime(date_breaks = no_of_pips_string) + 
        scale_y_continuous(breaks = (pretty_breaks()))
}