### Generate X matrix
makeBasis.slearner<- function(x,y, widths, export.constructor=TRUE, control=makeControl()){
  ## For debugging:
  #   load_mnist(dirname='../Data/mnist/')
  #   train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.2))
  #   x<- train$x[train.ind,]
  #   y<- train$y[train.ind]
  #   widths<- c(10,10,10)
  
  
  ## Sketch:
  # Choose first layer by singular values
  # Choose next layers to have good predictive power of validation residuals 
  # and linearly independent of all existing predictors.
  
  ## Initialization
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width value.")
  if(is.factor(y)) y<- as.numeric(as.character(y))
  
  
  
  ## Make first layer:
  x<- cbind(1, x)
  x.svd<- propack.svd(x, neig =widths[1])
  W<- x.svd$v
  x2 <- x %*% W # Low dimensional X representation (F in Ohad)
  #  checkOrtho(x2[,1:widths[1]]) # Orthogonal but not orthonormal
  W2 <- W / matrix(getNorm(x2), ncol=ncol(W), nrow=nrow(W), byrow=TRUE)
  x2 <- (x %*% W2) # Low dimensional X representation (F in Ohad)  
  x.2.ncol<- ncol(x2)
  # checkOrtho(x2)   # x2 Orthonormal
  
  
  x.added<- x.t<- x.t.orth<- x2 # The cummulating basis
  if(length(unique(y))>2)  y<- sapply(unique(y), function(x) as.numeric(y==x)) 
  
  y.t<- y
    
  y.os<- object.size(y)
  x.t.orth.os<- object.size(x.t.orth)
  
  if(y.os <= x.t.orth.os){
    y.t<- y.t -  x.t.orth %*% (t(x.t.orth) %*% y.t) # Residuals
  } else {
    y.t<- y.t -  (x.t.orth %*% t(x.t.orth)) %*% y.t # Residuals
  }
  
  
  cat(sprintf("Layer 1 done.\n"))
  
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
    
    
    x.candidate<- x.candidate - x.t.orth %*% (t(x.t.orth) %*% x.candidate) # Orthogonal candidate predictors 
    # round(t(x.candidate) %*% x.t, 3)
    # round(t(x.candidate) %*% x.t.orth, 3)
    x.candidate<- reNorm(x.candidate)
    
    
    x.added<- NULL
    x.added.indexes<- NULL
    j<- 1
    while(j <= widths[i]){
      x.candidate.cor<- abs(t(y.t) %*% x.candidate)
      x.candidate.ord<- which.max(getNorm(x.candidate.cor)) # Order along correlations
      
      if(x.candidate.cor[x.candidate.ord] < .Machine$double.eps) break()
      
      #   candidate.svd<- svd(cbind(x.candidate[, x.candidate.ord], x.added))
      #   candidate.svd<- propack.svd(cbind(x.candidate[, x.candidate.ord], x.added))
      #   candidate.rank<- sum(candidate.svd$d>.Machine$double.eps)
      candidate.rank<- rankMatrix(cbind(x.candidate[, x.candidate.ord], x.added), method="qrLINPACK" )
      
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
        
        ## TODO: Computation of outer product expensive! How to improve orthogonaliztion?
        y.t<- y.t- x.added1 %*% (t(x.added1) %*% y.t)
        
        
        cat(sprintf("Constructed variable %s in layer %s.\n", j, i))
        flush.console()
        j<- j+1        
      }       
    }
    
    
    bestest[[i]]<- x.added.indexes
    
    # rankMatrix(x.added,  method="qrLINPACK" )
    # diag(checkOrtho(x.added))
    # round(t(x.added) %*% x.t,5)
    
    x.t<- cbind(x.t, x.added, deparse.level=0)
    # x.t.names<- c(x.t.names, x.added.names)
    # checkOrtho(x.t)
    
    x.added.svd<- propack.svd(x.added)
    
    x.t.orth<- cbind(x.t.orth, x.added.svd$u %*% t(x.added.svd$v) )
    
  }
  
  
  
  
  ### Return basis creating function:
  makeBasis<- NA
  if(export.constructor){
    
    makeBasis<- function(x){
      x0.2<- cbind(1, x)
      
      ## For debugging:
      # x0.2<- x
      
      x2.2 <- x0.2 %*% W2 # Low dimensional X representation (F in Ohad)  
      
      x.t.2<- x2.2
      x.added.2<- x2.2
      
      x.t.2.svd<- propack.svd(x.t.2)
      x.added.ortho.2<- x.t.ortho.2<- x.t.2.svd$u %*% t(x.t.2.svd$v)
      # checkOrtho(x.t.ortho.2)
      
      for (i in 2:length(widths)){
        # i<-2
        x.candid.ind<- rep(1:ncol(x.added.2), each=x.2.ncol)
        x2.candid.inds<- rep(1:x.2.ncol, times=ncol(x.added.2))
        x.added.2<- x.added.2[,bestest[[i]][,'add']] * x2.2[ ,bestest[[i]][,'x2']]  # Reconstruct predictor
        
        
        # checkOrtho(x.t.ortho.2)
        x.added.2<- x.added.2 - x.t.ortho.2 %*% (t(x.t.ortho.2) %*% x.added.2)  
        x.added.2<- reNorm(x.added.2)
        #round(crossprod(x.added.2, x.t.ortho.2),5)
        #round(crossprod(x.added.2, x.t.2),5)
        #checkOrtho(x.added.2)
        
        x.added.svd.2<- propack.svd(x.added.2, opts=list(verbose=FALSE))
        x.added.ortho.2<- x.added.svd.2$u %*% t(x.added.svd.2$v)
        # checkOrtho(x.added.ortho.2)
        # round(crossprod(x.added.ortho.2, x.t.ortho.2), 4) # Orthonormalization breaks orthogonality?!?
        
        x.t.ortho.2<- cbind(x.t.ortho.2,  x.added.ortho.2)
        # checkOrtho(x.t.ortho.2)
        
        x.t.2<- cbind(x.t.2, x.added.2)
      }
      
      #anova(lm(y[,1]~x.t[,1:9]))
      #anova(lm(y[,1]~x.t.2[,1:9]))
      
      return(x.t.2)    
      # Use terms.matrix mechanism to make predictors
    }
    
    
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
# widths<- rep(3,4)
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
# load(file='Package/data/test.data.RData')
# widths<- c(10,10)
# .test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
# sum(widths)
# dim(.test$basis)
# 
# train.ind<- rep(FALSE, nrow(x))
# train.ind[1:250]<- TRUE
# .test<- makeBasis.slearner(x=test.data$X[train.ind,], y=test.data$Y[train.ind], widths=widths)





## SVM using the basis:
svm.slearner<- function(x, y, widths, train.ind, lambdas=2^(1:6), 
                        tunecontrol=tune.control(sampling="fix"), ... ){
  stopifnot(isTRUE(nrow(x)==length(y)))
  
  if(missing(train.ind)) train.ind<- as.logical(rbinom(nrow(x), 1, 0.3))
  
  ### create basis=
  ## TODO: Avoid overfit by optimizing training set (allow subset or crossvalidate)
  xx<- makeBasis.slearner(x=x[train.ind,], y=y[train.ind], widths=widths)
  xxx<- xx$makeBasis(x)
  ### fit model:
  # Add "type='C'" if svm does not correctly recognize the type:
  svm.tune <- tune.svm(x=xxx, y=y, kernel='linear', cost = lambdas, ...)
  
  
  result<-list(fit=svm.tune, makeBasis=xx$makeBasis)
  class(result)<- c("slearner","list")
  return(result)
}
## Testing:
## Replicating Ohad's example:
# load(file='Package/data/test_data.RData')
# widths<- c(10,10)
# lambdas<-  2^seq(-10,3,length=50) 
# train.ind<- rep(FALSE, nrow(test.data$X))
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
predict.slearner<- function(object, newdata,...){
  if(missing(newdata)) {
    preds<- predict(object$fit$best.model, ...)
  } else{
    new.x<- object$makeBasis(newdata)
    preds<- predict(object$fit$best.model, newdata=new.x, ...)  
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









summary.slearner<- function(object, ...){
  cat(rep("#",10), "  Tunning Summary  ", rep("#",10), "\n")
  print(summary(object$fit))
  cat(rep("#",10), "  Best Model  ", rep("#",10), "\n")
  summary(object$fit$best.model)
}
## Testing
# summary.slearner(slearner)

