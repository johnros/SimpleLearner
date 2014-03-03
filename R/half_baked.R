
svm.slearner<- function(x, y, widths, train.ind, type=c("fix","cross"), lambdas=2^(1:6), ... ){
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
  if(missing(train.ind)) train.ind<- as.logical(rbinom(nrow(x), 1, 0.7))
  
  
  ### create basis=
  xx<- makeBasis.slearner(x=x[train.ind,], y=y[train.ind], widths=widths)
  xxx<- xx$makeBasis(x)
  
  
  ### fit model:
  
  # Cross validatd version
  switch(type,
          fix={
            Liblinear.i<- list()
            Liblinear.miscalss<- rep(NA, length=length(lambdas))
            for(i in seq(along.with=lambdas)){
              # i<- 1
              .temp<- LiblineaR(data=xxx[train.ind,], labels=y[train.ind], type=4, cost=lambdas[i], cross=0)
              Liblinear.i<- c(Liblinear.i, .temp)
              
              .temp.predict<- predict(.temp, newx=xxx[!train.ind,])$predictions
              misclass<- table(.temp.predict, true=y[!train.ind])
              misclass.prop<- prop.table(misclass)
              diag(misclass.prop)<- 0
              Liblinear.miscalss[i]<- sum(misclass.prop)        
            }
            min.ind<- which.min(Liblinear.miscalss)
          },
          cross={
            Liblinear.i<- list()
            for(i in seq(along.with=lambdas)){
              .temp<- LiblineaR(data=xxx, labels=y, type=4, cost=lambdas[i], cross=2)
              Liblinear.i<- c(Liblinear.i, .temp)
            }  
            min.ind<- which.min(sapply(Liblinear.i, function(x) 1-x))
          }
         ) # end switch
  
  Liblinear.1<- LiblineaR(data=xxx, labels=y, type=4, cost=lambdas[min.ind], cross=0)
    
  result<-list(
    fit=Liblinear.1, 
    lambda=lambdas[min.ind],
    makeBasis=xx$makeBasis)
  
  class(result)<- c("slearner", "list")
  return(result)
}
## Testing:
# Replicating Ohad's example:
load(file='Package/data/test.data.RData')
widths<- c(10,10)
lambdas<-  2^seq(-10,3,length=50) 
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind, type="fix",
                            lambdas=lambdas, widths=widths)
slearner.fit$fit$
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,type="cross",
                            lambdas=lambdas, widths=widths)
slearner.fit$fit$

  
predict(slearner.fit, newx=test.data$X)
# 
# 
## MNIST data:
load_mnist(dirname='../Data/mnist/')
train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.3))
widths<- c(50,600,600)
x<- train$x
y<-train$y
lambdas<- 2^(2:9)

slearner.fit<- svm.slearner(x=x, y=y, train.ind=train.ind, type="fix",
                            lambdas=lambdas, widths=widths)
slearner.fit<- svm.slearner(x=x, y=y, train.ind=train.ind, type="cross",
                            lambdas=lambdas, widths=widths)


predict(slearner.fit, newx=x[1:10,])














# Replicating Ohad's example:
load(file='Package/data/test.data.RData')
widths<- c(10,10)
lambdas<-  2^seq(-10,3,length=50) 
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            lambdas=lambdas, widths=widths)
summary(slearner.fit)

predict(slearner.fit)




load_mnist(dirname='../Data/mnist/')
train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.2))
widths<- c(50,50,10)
.test<- makeBasis.slearner(x=train$x[train.ind,], y=train$y[train.ind], widths=widths)





