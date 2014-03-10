
# Testing with Ohad's data:
library(SimpleLearner)
load(file='Package/data/test.data.RData')
widths<- c(10,10)
lambdas<-  2^seq(-10,3,length=50) 
train.ind<- rep(FALSE, nrow(test.data$X))
train.ind[1:250]<- TRUE
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
predict(slearner.fit, newx=test.data$X)
predict(slearner.fit, newx=cbind(test.data$X,test.data$X))

slearner.fit<- svm.slearner(x=cbind(test.data$X,test.data$X), y=test.data$Y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths, )
slearner.fit$widths.returned
predict(slearner.fit, newx=cbind(test.data$X,test.data$X))

slearner.fit$call

summary(slearner.fit)


## Testing with MNIST data:

load_mnist(dirname='../Data/mnist/')
train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.2))
widths<- c(50,50,10)
.test<- makeBasis.slearner(x=train$x[train.ind,], y=train$y[train.ind], widths=widths)





