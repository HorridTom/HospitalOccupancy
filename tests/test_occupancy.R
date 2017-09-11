 test_that("Test Interval Occupancy", {
   #run occupancy_day
   
   test_occupancy1 <- occupancy_day("2012-01-01")
   #correct occupancy
   correct_occupancy1 <- 499
   #check results
   expect_equal(test_occupancy1, correct_occupancy1)
 })