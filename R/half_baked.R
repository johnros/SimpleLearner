





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