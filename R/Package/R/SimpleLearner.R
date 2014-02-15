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
  x.2.ncol<- ncol(x2)
  # checkOrtho(x2)   # x2 Orthonormal
  
  
  x.added<- x.t<- x.t.orth<- x2 # The cummulating basis
  y.t<- y
  y.t<- y.t - x.t.orth %*% t(x.t.orth) %*% y.t # Residuals
  # x.t.names<- x.added.names<- x2.names<- sprintf("%d", 1:ncol(x2))
  bestest<- list()
  for(i in 2:length(widths)){
    #i<- 2
    ## Sketch:
    # Update residuals
    # compute candidates
    # Remove orthogonal parts of Xs
    # sort variables by correlation
    # Add layer
    
    x.t.candid.ind<- rep(1:ncol(x.added), each=x.2.ncol)
    x2.candid.inds<- rep(1:x.2.ncol, times=ncol(x.added))
    x.candidate<- x.added[, x.t.candid.ind] * x2[, x2.candid.inds] # Candidate predictors
    x.candidate<- x.candidate - x.t.orth %*% t(x.t.orth) %*% x.candidate # Orthogonal candidate predictors 
    # round(t(x.candidate) %*% x.t, 3)
    # round(t(x.candidate) %*% x.t.orth, 3)
    x.candidate<- reNorm(x.candidate)
    
    x.added<- NULL
    x.added.indexes<- NULL
    j<- 1
    while(j <= widths[i]){
      x.candidate.cor<- abs(t(y.t) %*% x.candidate)
      x.candidate.ord<- which.max(x.candidate.cor) # Order along correlations
      
      if(x.candidate.cor[x.candidate.ord] < .Machine$double.eps) break()
      
      candidate.svd<- svd(cbind(x.candidate[, x.candidate.ord], x.added))
      candidate.rank<- sum(candidate.svd$d>.Machine$double.eps)
      
      if(j>1 && candidate.rank < (ncol(x.added)+1)){ 
        # In case of linear dependance
        x.candidate[, x.candidate.ord]<- 0
      } else {        
        # In case of linear independance
        x.added1<- x.candidate[, x.candidate.ord] # Select the best candidates
        # Update cummulative basis
        x.added<- cbind(x.added, x.added1)
        # x.added.names<- c(x.added.names, x.candidate.names[x.candidate.ord])
        
        x.added.indexes<- rbind(x.added.indexes, c( 
          add=x.t.candid.ind[x.candidate.ord],
          x2=x2.candid.inds[x.candidate.ord] ))
        
        y.t<- y.t- x.added1 %*% t(x.added1) %*% y.t          
        j<- j+1        
      }       
    }
    
    bestest[[i]]<- x.added.indexes
    # print(rankMatrix(x.added))
    
    # diag(checkOrtho(x.added))
    # round(t(x.added) %*% x.t,5)
        
    x.t<- cbind(x.t, x.added, deparse.level=0)
    # x.t.names<- c(x.t.names, x.added.names)
    # checkOrtho(x.t)
    
    x.added.svd<- svd(x.added)
    x.t.orth<- cbind(x.t.orth, x.added.svd$u %*% t(x.added.svd$v) )
  }
  
  
  
  
  ### Return basis creating function:
  makeBasis<- function(x){
    x0.2<- cbind(1, x)
    x2.2 <-(x0.2 %*% W2)[,1:widths[1]] # Low dimensional X representation (F in Ohad)  
    # checkOrtho(x2)   #
      
    x.t.2<- x.t.ortho.2<- x2.2
    x.added.ortho.2<- x.added.2<- x2.2
    for (i in 2:length(widths)){
      # i<-2 
      x.candid.ind<- rep(1:ncol(x.added.2), each=x.2.ncol)
      x2.candid.inds<- rep(1:x.2.ncol, times=ncol(x.added.2))
      x.added.2<- x.added.2[,bestest[[i]][,'add']] * x2.2[ ,bestest[[i]][,'x2']]  # Reconstruct predictor
    
      
      checkOrtho(x.t.ortho.2)
      x.added.2<- x.added.2 - x.t.ortho.2 %*% t(x.t.ortho.2) %*% x.added.2  
      x.added.2<- reNorm(x.added.2)
      round(crossprod(x.added.2, x.t.ortho.2),5)
      round(crossprod(x.added.2, x.t.2),5)
      checkOrtho(x.added.2)
      
      x.added.svd.2<- svd(x.added.2)
      x.added.ortho.2<- x.added.svd.2$u %*% t(x.added.svd.2$v)
      # checkOrtho(x.added.ortho.2)
      # round(crossprod(x.added.ortho.2, x.t.ortho.2), 4) # Orthonormalization breaks orthogonality?!?
      
      x.t.ortho.2<- cbind(x.t.ortho.2,  x.added.ortho.2)
      # checkOrtho(x.t.ortho.2)
      
      x.t.2<- cbind(x.t.2, x.added.2)
    }
        
    #anova(lm(y~x.t[,1:9]))
    #anova(lm(y~x.t.2[,1:9]))
    
    return(x.t.2)    
    # Use terms.matrix mechanism to make predictors
  }
      
  return(list(
    basis=x.t,
    makeBasis=makeBasis))
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
# .test.makeBasis<- .test$makeBasis(x)
# anova(lm(y~.test$basis))
# anova(lm(y~.test.makeBasis))
# 
# load(file='Package/data/test_data.RData')
# widths<- c(10,10)
# .test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
# sum(widths)
# dim(.test$basis)
# 
# train.ind<- rep(FALSE, nrow(x))
# train.ind[1:250]<- TRUE
# .test<- makeBasis.slearner(x=test.data$X[train.ind,], y=test.data$Y[train.ind], widths=widths)





## SVM using the basis:
svm.slearner<- function(x, y, widths, train.ind, lambdas=2^(1:6), control=makeControl(), ... ){
  stopifnot(isTRUE(nrow(x)==length(y)))
  
  if(missing(train.ind)) train.ind<- as.logical(rbinom(nrow(x), 1, 0.5))
  
  ### create basis=
  ## TODO: Avoid overfit by optimizing training set (allow subset or crossvalidate)
  xx<- makeBasis.slearner(x=x[train.ind,], y=y[train.ind], widths=widths)
  xxx<- xx$makeBasis(x)
  ### fit model:
  # Add "type='C'" if svm does not correctly recognize the type:
  svm.tune <- tune.svm(x=xxx, y=y, 
                       kernel='linear', cost = lambdas, 
                       tunecontrol=control$tunecontrol)
  result<-list(fit=svm.tune,
               makeBasis=xx$makeBasis)
  class(result)<- c("slearner","list")
  return(result)
}
## Testing:
## Replicating Ohad's example:
# load(file='Package/data/test_data.RData')
# widths<- c(10,10)
# lambdas<-  2^seq(-10,3,length=50) 
# undebug(svm.slearner)
# train.ind[1:250]<- TRUE
# slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
#                             lambdas=lambdas, widths=widths, 
#                             control=makeControl(sampling="fix"))
# predict(slearner.fit)
# 
# 
## Random training set:
# train.ind<- as.logical(rbinom(nrow(test.data$X), 1, 0.5))
# slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
#                             lambdas=lambdas, widths=widths, 
#                             control=makeControl(sampling="fix"))
# slearner.fit
# 
# ## Test with my data:
# x.p<- 5
# x<- matrix(rnorm(10000), 1000, x.p, dimnames=list(NULL, LETTERS[1:x.p]))
# x.framed<- as.data.frame(x)
# .xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
# y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
# y.factor<- factor(sign(y))
# widths<- rep(5,10)
# lambdas<- 2^(1:6)
# 
# slearner.fit<- svm.slearner(x=x, y=y.factor, widths=widths)
# predict(slearner.fit$$fit$best.model)






## Predict
predict.slearner<- function(slearner, newdata,...){
  if(missing(newdata)) {
    preds<- predict(slearner$fit$best.model, ...)
  } else{
    new.x<- slearner$makeBasis(newdata)
    preds<- predict(slearner$fit$best.model, newdata=new.x)  
  }  
  return(preds)
}
### Testing:
# ## Random training set:
# x.p<- 5
# x<- matrix(rnorm(10000), 1000, x.p, dimnames=list(NULL, LETTERS[1:x.p]))
# x.framed<- as.data.frame(x)
# .xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
# y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
# y.factor<- factor(sign(y))
# widths<- rep(5,10)
# slearner<- svm.slearner(x=x, y=y.factor, widths=widths)
# str(slearner)
# slearner$fit
# newdata<- x
# predict.slearner(slearner)
# predict.slearner(slearner, newdata=newdata)









summary.slearner<- function(slearner){
  cat(rep("#",10), "  Tunning Summary  ", rep("#",10), "\n")
  print(summary(slearner$fit))
  cat(rep("#",10), "  Best Model  ", rep("#",10), "\n")
  summary(slearner$fit$best.model)
}
## Testing
# summary.slearner(slearner)

