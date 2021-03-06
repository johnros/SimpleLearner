### Generate X matrix
makeBasis.slearner<- function(x,y, widths, export.constructor=TRUE, 
                              svd.method='exact', control=makeControl()){
  ## For debugging:
  #     load(file='Package/data/test.data.RData')
  #     widths<- c(17,10)
  #     lambdas<-  2^seq(-10,3,length=50) 
  #     train.ind<- rep(FALSE, nrow(test.data$X))
  #     train.ind[1:250]<- TRUE
  #     x<- cbind(test.data$X,test.data$X)
  #     y<- test.data$Y
  ff<- tempfile('Slearner_log_')
  fff<- file(ff, open="wt")
  sink(fff, type='message')
  
  
  ## Sketch:
  # Choose first layer by singular values
  # Choose next layers to have good predictive power of validation residuals 
  # and linearly independent of all existing predictors.
  
  ## Initialization
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width values.")
  if(is.factor(y)) y<- as.numeric(as.character(y))
  widths.returned<- rep(NA, length=length(widths))
  
  
  ## Make first layer:
  
  
  x<- cbind(1, x)
  ## TODO: B)compute rank without creating a copy of x.
  
  
  ## Note: the irlba approximation greatly deteriorates classifications
  switch(svd.method,
         exact={
           rank.layer1<- min(rankMatrix(x, method="qrLINPACK"), widths[1])
           x.svd<- propack.svd(x, neig=rank.layer1)
         },
         approx={
           x.svd<- irlba(x, nu=widths[1], sigma='ls') 
           }
         )


  

  
  
  
  
  x2 <- x %*% x.svd$v # Low dimensional X representation (F in Ohad)
  #  checkOrtho(x2[,1:widths[1]]) # Orthogonal but not orthonormal
  W2 <- x.svd$v / matrix(getNorm(x2), ncol=ncol(x.svd$v), nrow=nrow(x.svd$v), byrow=TRUE)
  x2 <- (x %*% W2) # Low dimensional X representation (F in Ohad)  
  x.2.ncol<- ncol(x2)
  # checkOrtho(x2)   # x2 Orthonormal
  
  widths.returned[1]<- x.2.ncol
  
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
        
        ## TODO: Computation of inner product expensive! How to improve orthogonaliztion?
        y.t<- y.t- x.added1 %*% (t(x.added1) %*% y.t)
        
        
        cat(sprintf("Constructed variable %s in layer %s.\n", j, i))
        flush.console()
        j<- j+1        
      }       
    }
    
    widths.returned[i]<- j-1
    bestest[[i]]<- x.added.indexes
    
    # rankMatrix(x.added,  method="qrLINPACK" )
    # diag(checkOrtho(x.added))
    # round(t(x.added) %*% x.t,5)
    
    x.t<- cbind(x.t, x.added, deparse.level=0)
    # x.t.names<- c(x.t.names, x.added.names)
    # checkOrtho(x.t)
    
    x.added.svd<- propack.svd(x.added)
    #x.added.svd<- irlba(x.added)
    
    x.t.orth<- cbind(x.t.orth, x.added.svd$u %*% t(x.added.svd$v) )
    
  }
  
  # Return basis creating function:
  makeBasis<- NA
  if(export.constructor){
    
    
    cat("\nExporting basis constructor function. \n In the meanwhile, enjoy a fortune():\n")
    print(fortune())
    
    makeBasis<- function(x){
      x0.2<- cbind(1, x)
      
      ## For debugging:
      # x0.2<- x
      if(ncol(x0.2)!= nrow(W2)) stop('Wrong dimensions for X matrix')
      x2.2 <- x0.2 %*% W2 # Low dimensional X representation (F in Ohad)  
      
      x.t.2<- x2.2
      x.added.2<- x2.2
      
      x.t.2.svd<- propack.svd(x.t.2)
      #x.t.2.svd<- irlba(x.t.2, nu=ncol(x.t.2), nv=ncol(x.t.2))
      
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
        
        x.added.svd.2<- propack.svd(x.added.2)
        #x.added.svd.2<- irlba(x.added.2)        
        
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
  
  
  sink(type='message')
  
  
  return(list(
    basis=x.t,
    widths.returned=widths.returned,
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







svm.slearner<- function(x, y, widths, train.ind, 
                        type, lambdas=2^(1:6), folds=2, svd.method='exact', ... ){
  ## For Debugging:
  #   load_mnist(dirname='../Data/mnist/')
  #   train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.7))
  #   widths<- c(50,600,600)
  #   x<- train$x
  #   y<-train$y
  #   lambdas<- 2^(2:9)
  #   
  
  ## Initialization: 
  stopifnot(isTRUE(nrow(x)==length(y)))
  if(missing(train.ind)) train.ind<- as.logical(rbinom(nrow(x), 1, 0.5))
  
  ### create basis=
  xx<- makeBasis.slearner(x=x[train.ind,], y=y[train.ind], widths=widths, svd.method=svd.method)
  xxx<- xx$makeBasis(x)
  
  
  ### fit model:
  
  
  
  # Cross validatd version
  Liblinear.type<- ifelse(length(unique(y))>2, 4, 1)
  switch(type,
         fix={
           Liblinear.i<- list()
           Liblinear.miscalss<- rep(NA, length=length(lambdas))
           
           ##TODO: Consider lapply instead of loop to deal with multiple copies of xxx.
           ##TODO: Can loop be parallelized without multiple copies of xxx?
           for(i in seq(along.with=lambdas)){
             # i<- 1
             .temp<- LiblineaR(data=xxx[train.ind,], labels=y[train.ind], 
                               type=Liblinear.type, cost=lambdas[i], cross=0, ...)
             
             Liblinear.i<- c(Liblinear.i, .temp)
             
             .temp.predict<- predict(.temp, newx=xxx[!train.ind,])$predictions
             Liblinear.miscalss[i]<- mean(.temp.predict!=y[!train.ind])        
           }
           min.ind<- which.min(Liblinear.miscalss)
           cv.misclass<- Liblinear.miscalss[[min.ind]]
           
         },
         
         
         cross={
           Liblinear.i<- list()
           for(i in seq(along.with=lambdas)){
             .temp<- 1- LiblineaR(data=xxx, labels=y, type=Liblinear.type, cost=lambdas[i], cross=folds,...)
             Liblinear.i<- c(Liblinear.i, .temp)
           }  
           min.ind<- which.min(sapply(Liblinear.i, function(x) x))
           cv.misclass<- Liblinear.i[[min.ind]]
         }
  ) # end switch
  
  Liblinear.1<- LiblineaR(data=xxx, labels=y, type=4, cost=lambdas[min.ind], cross=0,...)
  
  result<-list(
    call=match.call(),
    fit=Liblinear.1,
    lambdas=lambdas,
    type=type,
    lambda=lambdas[min.ind],
    CV.misclass=cv.misclass,
    widths.requested=widths,
    widths.returned=xx$widths.returned,
    makeBasis=xx$makeBasis)
  
  class(result)<- c("slearner", "list")
  return(result)
}
## Testing:
# Replicating Ohad's example:
# load(file='Package/data/test.data.RData')
# widths<- c(10,10)
# lambdas<-  2^seq(-10,3,length=50) 
# train.ind<- rep(FALSE, nrow(test.data$X))
# train.ind[1:250]<- TRUE
# slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind, type="fix",
#                             lambdas=lambdas, widths=widths)
# slearner.fit$fit$
#   slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,type="cross",
#                               lambdas=lambdas, widths=widths)
# slearner.fit$fit$
#  
## MNIST data:
# load_mnist(dirname='../Data/mnist/')
# train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.3))
# widths<- c(50,600,600)
# x<- train$x
# y<-train$y
# lambdas<- 2^(2:9)
# 
# slearner.fit<- svm.slearner(x=x, y=y, train.ind=train.ind, type="fix",
#                             lambdas=lambdas, widths=widths)
# slearner.fit<- svm.slearner(x=x, y=y, train.ind=train.ind, type="cross",
#                             lambdas=lambdas, widths=widths)








## Predict
predict.slearner<- function(object, newx, ...){
  new.xxx<- object$makeBasis(newx)
  preds<- predict(object$fit, new.xxx, ...)  
  
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









summary.slearner<- function(object,...){
  #object<- slearner.fit
  ## Call
  cat("\nCall:\n", paste(deparse(object$call), sep="\n", collapse = "\n"), "\n", sep = "")
  ## Layers requestd
  cat("\nLayers requested: ", paste(object$widths.requested, sep="\n", collapse = ","), "\n", sep = "")
  ## Layers returned
  cat("\nLayers learned: ", paste(object$widths.returned, sep="\n", collapse = ","), "\n", sep = "")
  ## lambdas scanned
  cat("\nLambdas considered: ", zapsmall(object$lambdas),"\n")
  ## Lambda chosen
  cat("\nLambda selected: ", object$lambda,"\n")
  ## CV accuracy:
  cat("\nTest misclassification: ", object$CV.misclass,"\n")
  ### LiblineaR summary
  # Number of classes:
  cat("\nNumber of classes to learn: ",object$fit$NbClass,"\n")
}
## Testing
# summary(slearner)
