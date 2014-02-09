

## Create control parameters 
genControl.slearner<- function(){
  
}


### Generate X matrix
## Sketch:
# Choose first layer by singular values
# Choose next layers to have good predictive power of validation residuals 
# and linearly independent of all existing predictors.
makeBasis.slearner<- function(x,y, widths){
  # Checks:
  stopifnot(all(widths <= head(c(1,widths) * ncol(x),-1)))
  
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
  
  
  .data<- as.data.frame(cbind(y, x1, x0))
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
    lm.2<- step(object=lm.2, direction='forward', scope=form.up, steps=widths[i])
    }
  anova(lm.2)
  
  return(model.matrix(lm.2))
}
## Testing:
x<- matrix(rnorm(10000),1000,5)
x.framed<- as.data.frame(x)
xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed) 
y<- xx %*% runif(ncol(xx), 0, 30)  + rnorm(nrow(xx), sd=2)
widths<- rep(5,10)

.test<- makeBasis.slearner(x=x, y=y, widths=widths)
sum(widths)
dim(.test)
colnames(.test)






## Fit model
fit.slearner<- function(x, y, widths){
  ### create basis
  
  
  
  ### fit model
}


## Predict
predict.slearner<- function(){
  
}

summary.slearner<- function(){
  
}

