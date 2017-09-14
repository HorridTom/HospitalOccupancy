
# Loads data files into Global Environment
# Note: this will overwrite objects with the same names
# in the Global Environment.
load_data <- function() {
  load("data/admission_data_clean.rda", .GlobalEnv)
  load("data/emergency_adms.rda", .GlobalEnv)
}


# loads libraries
load_libraries <- function(){
  library(lubridate)
  library(ggplot2)
  library(testthat)
  library(reshape2)
  library(scales)
}