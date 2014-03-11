## Synthetic data with line profiling:
rm(list=ls())
library(SimpleLearner)
x.p<- 5
x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
colnames(x.framed<- as.data.frame(x))
colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
widths<- rep(3,4)

library(lineprof)
prof.learner<- lineprof(makeBasis.slearner(x=x, y=y, widths=widths))
shine(prof.learner)




# Testing with Ohad's data:
rm(list=ls())
library(SimpleLearner)
load(file='Package/data/test.data.RData')
widths<- c(10,20)
lambdas<-  2^seq(1,10,length=50) 
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE

slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=test.data$X)
summary(slearner.fit)
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="cross", lambdas=lambdas, widths=widths)
summary(slearner.fit)

slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind, 
                            type="fix", svd.method='approx', lambdas=lambdas, widths=widths)
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind, 
                            type="fix", svd.method='exact', lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=test.data$X)
summary(slearner.fit)



library(lineprof)
prof.learner<- lineprof(svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind, type="fix", lambdas=lambdas, widths=widths))
shine(prof.learner)



## Testing with MNIST data:
rm(list=ls())
load_mnist(dirname='../Data/mnist/')
train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.2))
widths<- c(50,600,600)
widths<- c(10,10,10)
lambdas<-  2^seq(-2,6,length=50) 

slearner.fit<- svm.slearner(x=train$x[train.ind,], y=train$y[train.ind],
                            type="fix", svd.method='exact', lambdas=lambdas, widths=widths)
slearner.fit<- svm.slearner(x=train$x[train.ind,], y=train$y[train.ind],
                            type="fix", svd.method='approx', lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=train$x)
summary(slearner.fit)


slearner.fit<- svm.slearner(x=train$x, y=train$y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=train$x)
summary(slearner.fit)

library(lineprof)
prof.learner<- lineprof(
  svm.slearner(x=train$x[train.ind,], y=train$y[train.ind], type="fix", 
               lambdas=lambdas, widths=widths),
  interval=0.1)
shine(prof.learner)



slearner.fit<- svm.slearner(x=train$x[train.ind,], y=train$y[train.ind],
                            type="cross", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=train$x)
summary(slearner.fit)




