
svm.slearner<- function(x, y, widths, train.ind, lambdas=2^(1:5), 
                        tunecontrol=tune.control(sampling="fix"), ... ){
  ## For Debugging:
  load_mnist(dirname='../Data/mnist/')
  train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.2))
  widths<- c(10,10,10)
  x<- train$x
  y<-train$y
  
  lambdas<- 2^(1:3)
  
  
  ## Initialization: 
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

















load(file='Package/data/test.data.RData')
widths<- c(10,10)
.test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
sum(widths)
dim(.test$basis)



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





