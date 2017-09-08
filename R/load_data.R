
# Either return the argument x or raise it to the power 4

my_function <- function(x, ident = FALSE) {
  
  if(ident){ 
    y <- x
  } else { 
    y <- x^4
  }
  
  y
}

# Loads data files into Global Environment
# Note: this will overwrite objects with the same names
# in the Global Environment.
load_data <- function() {
  load("data/admission_data_clean.rda", .GlobalEnv)
  load("data/emergency_adms.rda", .GlobalEnv)
}