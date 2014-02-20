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


