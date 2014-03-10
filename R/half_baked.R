

# Testing with Ohad's data:
rm(list=ls())
library(SimpleLearner)
load(file='Package/data/test.data.RData')
widths<- c(10,10)
lambdas<-  2^seq(1,6,length=50) 
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=test.data$X)
summary(slearner.fit)
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="cross", lambdas=lambdas, widths=widths)
summary(slearner.fit)


## Testing with MNIST data:
rm(list=ls())
load_mnist(dirname='../Data/mnist/')
train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.7))
widths<- c(50,600,600)
widths<- c(50,10,10)
lambdas<-  2^seq(-2,6,length=50) 
slearner.fit<- svm.slearner(x=train$x, y=train$y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=train$x)
summary(slearner.fit)





### Old version using svm.tune (e1071)
svm.slearner.old<- function(x, y, widths, train.ind, lambdas=2^(1:6), control=makeControl(), ... ){
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
  result<-svm.tune 
  
  return(result)
}
## Testing:
## Replicating Ohad's example:
library(SimpleLearner)
load(file='Package/data/test.data.RData')
widths<- c(10,10)
lambdas<-  2^seq(1,6,length=50) 
train.ind<- rep(FALSE, nrow(test.data$x))
train.ind[1:250]<- TRUE

load(file='Package/data/test_data.RData')
widths<- c(10,10)
lambdas<-  2^seq(-10,3,length=50) 
debug(svm.slearner)
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            lambdas=lambdas, widths=widths, 
                            control=makeControl(sampling="fix"))
slearner.fit


