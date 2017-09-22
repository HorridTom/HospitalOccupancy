#test for hospital occupancy function 
#require(lubridate)

test_that("Test hospital_occupancy", {
   
   #set up a much smaller data frame with known occupancies at given test_times
   test_pseudoID <- c(1,1,2,3,4,5)
   test_CSPAdmissionTime <- c("2011-01-01 00:00:00","2011-01-01 00:00:00", "2011-01-02 00:00:00", "2011-01-01 00:00:00", "2011-01-05 00:00:00", "2011-01-05 00:00:00")
   test_CSPDischargeTime <- c("2011-01-03 12:00:00","2011-01-03 12:00:00", "2011-01-03 23:59:59 ", "2011-01-02 12:00:00", "2011-01-05 12:00:00", "2011-01-05 23:59:59")
   test_EpisodeNumber <- c(1,2,1,1,1,1)
   test_df <- data.frame(test_pseudoID, test_CSPAdmissionTime, test_CSPDischargeTime, test_EpisodeNumber)
   test_times <- c("2011-01-01 15:00:00:00", "2011-01-02 10:00:00", "2011-01-03 15:00:00", "2011-01-04 15:00:00", "2011-01-05 10:00:00")
   
   #run hospital occupancy
   test_occupancy1 <- as.numeric(sapply(test_times, hospital_occupancy, df = test_df, time_in_col = "test_CSPAdmissionTime", time_out_col = "test_CSPDischargeTime", episode_col = "test_EpisodeNumber"))
   
   #correct occupancy
   correct_occupancy1 <- c(2, 3, 1, 0, 2)
   
   #check results
   expect_equal(test_occupancy1, correct_occupancy1)
   
  })
 
 


#test for ward occupancy function
#require(lubridate)

test_that("Test ward_occupancy", {
  
  #set up a much smaller data frame with known occupancies at given test_times
  test_pseudoID <- 1:5
  test_EpisodeStartDate <- c("2011-01-01 00:00:00", "2011-01-02 00:00:00", "2011-01-01 00:00:00", "2011-01-05 00:00:00", "2011-01-05 00:00:00")
  test_EpisodeEndDate <- c("2011-01-03 12:00:00", "2011-01-03 23:59:59 ", "2011-01-02 12:00:00", "2011-01-05 12:00:00", "2011-01-05 23:59:59")
  test_ward <- c("BLUE_ward", "ORANGE_ward", "ORANGE_ward", "BLUE_ward", "BLUE_ward")
  test_df <- data.frame(test_pseudoID, test_EpisodeStartDate, test_EpisodeEndDate, test_ward)
  test_times <- c("2011-01-01 15:00:00:00", "2011-01-02 10:00:00", "2011-01-03 15:00:00", "2011-01-04 15:00:00", "2011-01-05 10:00:00")
  
  #run ward occupancy
  test_occupancy_orange <- as.numeric(sapply(test_times, ward_occupancy, ward = "ORANGE_ward", df = test_df, time_in_col = "test_EpisodeStartDate", time_out_col = "test_EpisodeEndDate", ward_col = "test_ward"))
  test_occupancy_blue <- as.numeric(sapply(test_times, ward_occupancy, ward = "BLUE_ward", df = test_df, time_in_col = "test_EpisodeStartDate", time_out_col = "test_EpisodeEndDate", ward_col = "test_ward"))
  
  #correct occupancy
  correct_occupancy_orange <- c(1, 2, 1, 0, 0)
  correct_occupancy_blue <- c(1, 1, 0, 0, 2)
  
  #check results
  expect_equal(test_occupancy_orange, correct_occupancy_orange)
  expect_equal(test_occupancy_blue, correct_occupancy_blue)
  
})



test_that("Test hosp_occ", {
  
  #set up a much smaller data frame with known occupancies at given test_times
  test_pseudoID <- c(1,1,2,3,4,5)
  test_CSPAdmissionTime <- c("2011-01-01 00:00:00","2011-01-01 00:00:00", "2011-01-02 00:00:00", "2011-01-01 00:00:00", "2011-01-05 00:00:00", "2011-01-05 00:00:00")
  test_CSPDischargeTime <- c("2011-01-03 12:00:00","2011-01-03 12:00:00", "2011-01-03 23:59:59 ", "2011-01-02 12:00:00", "2011-01-05 12:00:00", "2011-01-05 23:59:59")
  test_EpisodeNumber <- c(1,2,1,1,1,1)
  test_df <- data.frame(test_pseudoID, test_CSPAdmissionTime, test_CSPDischargeTime, test_EpisodeNumber)
  test_times <- c("2011-01-01 15:00:00:00", "2011-01-02 10:00:00", "2011-01-03 15:00:00", "2011-01-04 15:00:00", "2011-01-05 10:00:00")
  
  #run hospital occupancy
  test_occupancy_df <- hosp_occ("2011-01-01 00:00:00", "2011-01-05 23:59:59", df = test_df, time_in_col = "test_CSPAdmissionTime", time_out_col = "test_CSPDischargeTime", episode_col = "test_EpisodeNumber")
  test_occupancy1 <- c()
  
  for(i in 1:5){
  test_occupancy1[i] <- test_occupancy_df$occupancy_vect[test_occupancy_df$Time == test_times[i]]
  }
  
  #correct occupancy
  correct_occupancy1 <- c(2, 3, 1, 0, 2)
  
  #check results
  expect_equal(test_occupancy1, correct_occupancy1)
  
})