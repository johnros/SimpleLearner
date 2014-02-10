

## Create control parameters 
makeControl<- function(
  sampling="fix"
  ){
  ## Initializing:
  require(e1071)
    
  return(list(
    tunecontrol=tune.control(sampling=sampling)
    ))
}
## Testing:
#makeControl()




### Generate X matrix
## Sketch:
# Choose first layer by singular values
# Choose next layers to have good predictive power of validation residuals 
# and linearly independent of all existing predictors.
makeBasis.slearner<- function(x,y, widths, control){
  
  ## Checks:
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width value.")
  if(missing(control)) control<- makeControl() # currently deprecated
  
  ## Initializing 
  d<- length(widths)
  n<- nrow(x)
  p<- ncol(x)
  
  ## Make first layer:
  x1.svd<- svd(x)
  D<- diag( x1.svd$d)
  x0<-(x1.svd$u %*% D %*% t(x1.svd$v))
  x1<-x0[,1:widths[1]] 

  ## Make next layers:
  # Check for orthogonality with existing layers
  # Check for predictive power
  
  
  .data<- as.data.frame(cbind(y, x1, x0, deparse.level=0))
  x1.ind<- 1+(1:widths[1]) # Indexes of first layer
  xi.ind<- 1+(1:p) # Indexes of original X matrix
  form.low <- as.formula(paste('V1 ~',paste(sprintf("V%d", x1.ind), collapse="+")))
  lm.1<- lm(form.low, data=.data)
  lm.2<- lm.1
  
  for(i in 2:d){
    form.up.expression<- sprintf('V1 ~ (%s) * (%s)', 
                              deparse(formula(lm.2)[[3]], width.cutoff=500),
                              paste(sprintf("V%d", xi.ind), collapse=" + "))
    form.up<- as.formula(form.up.expression)  
    lm.2<- step(object=lm.2, direction='forward', scope=form.up, steps=widths[i], trace=0)
    }
  anova(lm.2)
  
  ## TODO: return rotation matrix to allow prediction on new data
  return(list(
    basis= model.matrix(lm.2)[,-1],
    svd=x1.svd ,
    formula=    ))
}
## Testing:ִ
debug(makeBasis.slearner)
x.p<- 5
x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
colnames(x.framed<- as.data.frame(x))
colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
widths<- rep(5,10)

.test<- makeBasis.slearner(x=x, y=y, widths=widths)
sum(widths)
dim(.test)
colnames(.test)

load(file='data/test_data.RData')
widths<- c(10,10)
.test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
sum(widths)
dim(.test)
colnames(.test)







## Fit model
svm.slearner<- function(x, y, widths, lambdas=2^(1:6), control=makeControl(sampling="fix"), ... ){
  
  ### create basis
  xx<- makeBasis.slearner(x=x, y=y, widths=widths)
  
  ### fit model:
  # Add "type='C'" if svm does not correctly recognize the type:
  svm.tune <- tune.svm(x=xx, y=y.factor, 
                  kernel='linear', cost = lambdas, 
                  tunecontrol=control$tunecontrol)
  result<-svm.tune 
  
  return(result)
}
## Testing:
## Remove:
x.p<- 5
x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
x.framed<- as.data.frame(x)
.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
y.factor<- factor(sign(y))
widths<- rep(5,10)
control<- makeControl()
lambdas<- 2^(1:6)

colnames(makeBasis.slearner(x=x, y=y, widths=widths))
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