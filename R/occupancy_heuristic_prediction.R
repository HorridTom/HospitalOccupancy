#the average occupancy by shift (8hr) and day of week from the previous 8 week period is used 
#to predict occupancy for future period
#good test line : predict_next_week("2012-05-01 00:00:00", df1 = sample_admission_data_clean, which_occ_function = "hosp_occ")

predict_next_week <- function(future_period_start_date, prediction_method = "OccDelta", df1 = ordered_times_df, which_occ_function = "hosp_occ"){
  
  prev_period_start_date <- as.Date(future_period_start_date) - weeks(8)
  prev_period_end_date <- as.Date(future_period_start_date) - hours(1)
  future_period_end_date <- as.Date(future_period_start_date) + weeks(1) - hours(2)
  
  #lets you choose which occupancy calculating function you want to use 
  if(which_occ_function == "hosp_occ"){
    budget_occupancy_df <- hosp_occ(prev_period_start_date, future_period_end_date, df1)
    budget_occupancy_vect <- budget_occupancy_df$occupancy_vect
  }else if(which_occ_function == "hospital_occupancy"){
    time <- seq(as.POSIXlt(prev_period_start_date), as.POSIXlt(future_period_end_date), by="hours")
    budget_occupancy_vect <- sapply(time, hospital_occupancy, df = df1)
  }else{
    print("Please enter either 'hosp_occ' or 'hospital_occupancy' in the which_occ_function argument")
  }
  
  #spits occupancies into shifts of 8hrs
  shifts_list <- split(budget_occupancy_vect, ceiling(seq_along(budget_occupancy_vect)/8))
  
  #finds the mean occupancy in each of these shifts
  shifts_mean_vect <- unlist(sapply(shifts_list, mean))
  
  no_of_shifts_in_1_week <- 21
  prev_shift_occ <- head(shifts_mean_vect, no_of_shifts_in_1_week*8)
  fut_shift_occ <- tail(shifts_mean_vect, no_of_shifts_in_1_week)

  #prediction
  if(prediction_method == "budget"){
    
    #collects shifts from the same day of the week at the same time (e.g. all 1st shift on Mondays are put together)
    title <- "Budget Occupancy Prediction"
    shift_mean_list <- lapply(c(1:no_of_shifts_in_1_week), get_shift_means, shifts_mean_vect1 = prev_shift_occ)
    shift_predictions <- sapply(shift_mean_list, mean)
    
  }else if(prediction_method == "OccDelta"){
    
    title <- "OccDelta Occupancy Prediction"
    deltas <- diff(prev_shift_occ)
    deltas_mean_list <- lapply(c(1:no_of_shifts_in_1_week), get_shift_means, shifts_mean_vect1 = deltas)
    deltas_predictions <- sapply(deltas_mean_list, mean)
    shift_predictions <- c()
    shift_predictions[1] <- prev_shift_occ[no_of_shifts_in_1_week*8] 
    
    for(i in 2:length(deltas_predictions)){
      shift_predictions[i] <- shift_predictions[i-1] + deltas_predictions[i-1]
    }
    
  }else{
    
    print("Please enter either 'budget' or 'OccDelta' in the which_occ_function argument")
  }
  
  #dfs to plot
  prev_time <- seq(as.POSIXlt(prev_period_start_date), as.POSIXlt(prev_period_end_date), by="8 hours")
  prev_occ_df <- data.frame(prev_time, prev_shift_occ)
  prediction_time <- seq(as.POSIXlt(future_period_start_date), as.POSIXlt(future_period_end_date), by="8 hours")
  prediction_df <- data.frame(prediction_time, shift_predictions)
  fut_occ_df <- data.frame(prediction_time, fut_shift_occ)

  ggplot() + geom_line(data=prev_occ_df, aes(x=prev_time, y=prev_shift_occ, colour="Previous_occupancies")) + 
       geom_line(data=prediction_df, aes(x=prediction_time, y=shift_predictions, colour="Predicted_occupancies")) +
       geom_line(data=fut_occ_df, aes(x=prediction_time, y=fut_shift_occ, colour="Real_occupancies")) +
       scale_colour_manual(name="Key", values=c(Previous_occupancies="blue", Predicted_occupancies="red", Real_occupancies="green"))+
       xlab("Shift Start Time") + ylab("Occupancy") + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
       theme(axis.text.x=element_text(angle=35, vjust=0.5)) + scale_x_datetime(date_breaks = "3 day")
  
}



get_shift_means <- function(shift_number, shifts_mean_vect1){
  
  no_of_shifts_in_1_week <- 21
  shift_mean_list <- list()
  shift_mean_list <- shifts_mean_vect1[seq(shift_number, length(shifts_mean_vect1), no_of_shifts_in_1_week)]
  shift_mean_list
}