## Utility functions:

.onAttach <- function(lib, pkg) {
  if (interactive()) {
    packageStartupMessage("SimpleLearner: An R implementation of Ohad Shamir's MATLAB code.\n",
      "Converted as part of Intel's 'Machine Learning for 2020 Project',\n",
      "and through the Intel and WIS ICRI-CI collaboration.\n",      
      domain=NA, appendLF=TRUE)
  }
}


getNorm<- function(A){
  apply(A, 2, function(x) sqrt(sum(x^2)))
}
## Testing
# getNorm(matrix(rnorm(100), 10,10))

reNorm<- function(A){
  A / matrix(getNorm(A), ncol=ncol(A), nrow=nrow(A), byrow=TRUE)
}
## Testing:
#reNorm(reNorm(matrix(rnorm(100), 10,10)))

checkOrtho<- function(A){
  round(t(A)%*%A, 4)
}
## Testing:
#checkOrtho(matrix(rnorm(1000),100,10))



## Create control parameters 
makeControl<- function(
  ){
  ## Initializing:
  
  
  return(list(    
  ))
}
## Testing:
#makeControl()
