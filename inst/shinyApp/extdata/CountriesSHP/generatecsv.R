fill.matrix = function(expr, nrow=1, ncol=1) {
  matrix(eval(expr, envir=list(x=nrow*ncol)), nrow=nrow, ncol=ncol)
}

random_pop_err <- function() {
 
  x <- matrix(nrow = 251, ncol = 2)
  
  #Loop through all cells of the matrix
  for(i in 1:nrow(x)) {
    
    for(j in 1:ncol(x)) {
      #Generate a random value between 0 and 1
      
      if(j == 1) {
        x[i,j] <- sample(1:10000000, 1)
      } else {
        x[i,j] <- sample(1:100, 1)
      }
    }
  }
  x
}


pop <- as.data.frame(random_pop_err())

pop$OBJECTID <- c(1:251)

colnames(pop) <- c("Estimate", "Error", "OBJECTID")

data.frame()
