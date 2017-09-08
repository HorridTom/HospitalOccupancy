
# Either return the argument x or raise it to the power 4
my_function <- function(x, ident = FALSE) {
  
  if(ident){ 
    y <- x
  } else { 
    y <- x^4
  }
  
  y
  
}