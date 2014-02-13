### Generate X matrix
## Sketch:
# Choose first layer by singular values
# Choose next layers to have good predictive power of validation residuals 
# and linearly independent of all existing predictors.
makeBasis.slearner<- function(x,y, widths, control){
  ## Delete Me:
#       x.p<- 5
#       n<- 100
#       x<- matrix(rnorm(n*x.p),n,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
#       x.framed<- as.data.frame(x)
#       .xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed)
#       y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
#       widths<- rep(4,10)
#       control<- makeControl()

  ## Checks:
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width value.")
  if(missing(control)) control<- makeControl() 
  if(is.factor(y)) y<- as.numeric(as.character(y))
  
  ## Make first layer:
  x0<- cbind(1, x)
  x0.svd<- svd(x0, nu=0, nv=ncol(x0))
  W<- x0.svd$v[ , 1:widths[1]]
  x1 <- x0 %*% W # Low dimensional X representation (F in Ohad)
  #  checkOrtho(x1[,1:widths[1]]) # Orthogonal but not orthonormal
  
  W2 <- W / matrix(getNorm(x1), ncol=ncol(W), nrow=nrow(W), byrow=TRUE)
  x2 <- (x0 %*% W2)[,1:widths[1]] # Low dimensional X representation (F in Ohad)  
  # checkOrtho(x2)   # x2 Orthonormal
  
  x.added<- x.t<- x.t.orth<- x2 # The cummulating basis
  y.t<- y
  y.t<- y.t - x.t.orth %*% t(x.t.orth) %*% y.t # Residuals
  
  for(i in 2:length(widths)){
    #i<- 3
    ## Sketch:
    # Update residuals
    # compute candidates
    # Remove orthogonal parts of Xs
    # sort variables by correlation
    # Add layer
    
    x.t.candid.ind<- rep(1:ncol(x.added), each=ncol(x1))
    x2.candid.inds<- rep(1:ncol(x2), times=ncol(x.added))
    ## TODO: manage column names to create predictor
        
    x.candidate<- x.added[, x.t.candid.ind] * x2[, x2.candid.inds] # Candidate predictors
    x.candidate<- x.candidate - x.t.orth %*% t(x.t.orth) %*% x.candidate # Orthogonal candidate predictors 
    # round(t(x.candidate) %*% x.t, 3)
    # round(t(x.candidate) %*% x.t.orth, 3)
    
    x.candidate<- reNorm(x.candidate)
        
    x.added<- NULL
    for(j in 1:widths[i]){
      x.candidate.cor<- abs(t(y.t) %*% x.candidate)
      x.candidate.ord<- which.max(x.candidate.cor) # Order along correlations
      if(x.candidate.cor[x.candidate.ord] < .Machine$double.eps) break()
      # abs(t(y.t) %*% x.candidate)[x.candidate.ord]
      x.added1<- x.candidate[, x.candidate.ord] # Select the best candidates
      #  checkOrtho(x.added); round(t(x.added) %*% x.t, 4); round(t(x.added) %*% x.t.orth, 4)
      
       # Update cummulative basis
      x.added<- cbind(x.added, x.added1)
      # checkOrtho(x.t)
            
      y.t<- y.t- x.added1 %*% t(x.added1) %*% y.t          
    }
    #     diag(checkOrtho(x.added))
    #     round(t(x.added) %*% x.t,5)
        
    # Note: Why does Ohad recompute the cadidate variables?
    x.t<- cbind(x.t, x.added, deparse.level=0)
    #     checkOrtho(x.t)
    
    x.added.svd<- svd(x.added)
    x.t.orth<- cbind(x.t.orth, x.added.svd$u %*% t(x.added.svd$v) )
    #     checkOrtho(x.t.orth)
    # diag(checkOrtho(x.candidate))
  }
      
  ## TODO: return rotation matrix and variable construction to allow prediction on new data
  return(list(
    basis=x.t,
    basis.ortho=x.t.orth))
}
## Testing:ִ
# x.p<- 5
# x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
# colnames(x.framed<- as.data.frame(x))
# colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
# y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
# widths<- rep(3,10)
# 
# .test<- makeBasis.slearner(x=x, y=y, widths=widths)
# sum(widths)
# dim(.test$basis)
# colnames(.test)
# 
# load(file='Package/data/test_data.RData')
# widths<- c(10,10)
# .test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
# sum(widths)
# dim(.test$basis)





