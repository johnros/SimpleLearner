## Fit model
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
  result<-svm.tune 
  
  return(result)
}
## Testing:
## Replicating Ohad's example:
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


## Random training set:
train.ind<- as.logical(rbinom(nrow(test.data$X), 1, 0.5))
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            lambdas=lambdas, widths=widths, 
                            control=makeControl(sampling="fix"))
slearner.fit

## Test with my data:
x.p<- 5
x<- matrix(rnorm(10000), 1000, x.p, dimnames=list(NULL, LETTERS[1:x.p]))
x.framed<- as.data.frame(x)
.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
y.factor<- factor(sign(y))
widths<- rep(5,10)
lambdas<- 2^(1:6)

slearner.fit<- svm.slearner(x=x, y=y.factor, widths=widths)
predict(slearner.fit$best.model)






## Predict
predict.slearner<- function(sleaner, newdata){
  if(missing(newdata)) return(predict(sleaner$best.model))
  
  ## TODO: apply predictor with new data.
  # Note: need to carry ratation matrix from makeBasis and assume variables have same ordering!
  
  ## In case new Xs are provided:
  # make new model.matrix
  # apply prediction with new model matrix
  varnames<- colnames(slearner.fit$best.model$SV)
  new.expression<- 
    new.formula<- as.formula(new.expression)
  model.matrix(new.formula, data=newdata)
  
  
}
## Testing:
sleaner.pred<- predict.slearner(slearner.fit)
## TODO: newdata predictin
sleaner.pred<- predict.slearner(slearner.fit)








summary.slearner<- function(){
  
}



## Plot SVM output
plot.slearner<- function