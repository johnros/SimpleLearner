## Utility functions:

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
