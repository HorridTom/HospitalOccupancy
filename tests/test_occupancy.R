#test for occupancy function 

test_that("Test Occupancy", {
   
   #set up a much smaller data frame with known occupancies at given test_times
   test_pseudoID <- 1:5
   test_CSPAdmissionTime <- c("2011-01-01 00:00:00", "2011-01-02 00:00:00", "2011-01-01 00:00:00", "2011-01-05 00:00:00", "2011-01-05 00:00:00")
   test_CSPDischargeTime <- c("2011-01-03 12:00:00", "2011-01-03 23:59:59 ", "2011-01-02 12:00:00", "2011-01-05 12:00:00", "2011-05-05 23:59:59")
   test_Stay <- interval(test_CSPAdmissionTime, test_CSPDischargeTime)
   test_df <- data.frame(test_pseudoID, test_CSPAdmissionTime, test_CSPDischargeTime, test_Stay)
   test_times <- c("2011-01-01 15:00:00:00", "2011-01-02 10:00:00", "2011-01-03 15:00:00", "2011-01-04 15:00:00", "2011-01-05 10:00:00")
   
   #run occupancy
   test_occupancy1 <- as.numeric(sapply(test_times, hospital_occupancy, df = test_df, df_col = "test_Stay"))
   
   #correct occupancy
   correct_occupancy1 <- c(2, 3, 1, 0, 2)
   
   #check results
   expect_equal(test_occupancy1, correct_occupancy1)
  })
 
 
 