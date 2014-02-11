## Fit model
svm.slearner<- function(x, y, widths, lambdas=2^(1:6), control=makeControl(sampling="fix"), ... ){
  
  ### create basis
  xx<- makeBasis.slearner(x=x, y=y, widths=widths)
  
  ### fit model:
  # Add "type='C'" if svm does not correctly recognize the type:
  svm.tune <- tune.svm(x=xx$basis, y=y, 
                       kernel='linear', cost = lambdas, 
                       tunecontrol=control$tunecontrol)
  result<-svm.tune 
  
  return(result)
}
## Testing:
x.p<- 5
x<- matrix(rnorm(10000), 1000, x.p, dimnames=list(NULL, LETTERS[1:x.p]))
x.framed<- as.data.frame(x)
.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
y.factor<- factor(sign(y))
widths<- rep(5,10)
control<- makeControl(sampling='fix')
lambdas<- 2^(1:6)

makeBasis.slearner(x=x, y=y, widths=widths)
slearner.fit<- svm.slearner(x=x, y=y.factor, widths=widths)
predict(slearner.fit$best.model)

## Testing with Ohad's data:
load(file='Package/data/test_data.RData')
widths<- c(10,10)
lambdas<- round( 2^seq(-10,2,length=30) , 4)
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, 
                            lambdas=lambdas, widths=widths, 
                            control=makeControl(sampling="fix"))
slearner.fit
slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, 
                            lambdas=0.01, widths=widths, 
                            control=makeControl(sampling="fix"))
slearner.fit





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