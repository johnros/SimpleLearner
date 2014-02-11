

## Create control parameters 
makeControl<- function(
  sampling="fix",
  penalty=2
  ){
  ## Initializing:
  require(e1071)
    
  return(list(
    tunecontrol=tune.control(sampling=sampling),
    penalty=penalty
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
  ## Delete Me:
#   x.p<- 5
#   x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
#   colnames(x.framed<- as.data.frame(x))
#   colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
#   y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
#   widths<- rep(4,10)
#   control=makeControl(penalty=0)
  
  ## Checks:
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width value.")
  if(missing(control)) control<- makeControl() # currently deprecated
  
  ## Initializing 
  d<- length(widths)
  n<- nrow(x)
  p<- ncol(x)
  
  ## Make first layer:
  x<- cbind(1, x)
  x1.svd<- svd(x)
  W<- x1.svd$v[ , 1:widths[1]]
  x0<-x %*% W # Low dimensinal X representation (F in Ohad)
  
  ## Make next layers:
  # Check for orthogonality with existing layers
  # Check for predictive power
  .data<- as.data.frame(cbind(y, x0, deparse.level=0)) # Note this will remove column names
  x1.ind<- 1+(1:widths[1]) # Indexes of first layer
  form.low.expression<- paste('V1 ~',paste(sprintf("V%d", x1.ind), collapse="+"))
  form.low <- as.formula(form.low.expression)
  lm.1<- lm(form.low, data=.data)
  lm.2<- lm.1
  
  for(i in 2:d){
    form.up.expression<- sprintf('V1 ~ (%s) * (%s)', 
                              deparse(formula(lm.2)[[3]], width.cutoff=500),
                              paste(sprintf("V%d", x1.ind), collapse=" + "))
    form.up<- as.formula(form.up.expression)  
    lm.2<- step(object=lm.2, direction='forward', scope=form.up, steps=widths[i], 
                trace=0, k=control$penalty)
    }
  #anova(lm.2)
    
  ## TODO: return rotation matrix to allow prediction on new data
  return(list(
    basis= model.matrix(lm.2)[,-1],
    svd=x1.svd,
    formula=NULL    ))
}
## Testing:ִ
# x.p<- 5
# x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
# colnames(x.framed<- as.data.frame(x))
# colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
# y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
# widths<- rep(3,10)
# 
# debug(makeBasis.slearner)
# .test<- makeBasis.slearner(x=x, y=y, widths=widths)
# sum(widths)
# dim(.test$basis)
# colnames(.test)
# 
# load(file='Package/data/test_data.RData')
# widths<- c(10,10)
# .test<- makeBasis.slearner(x=test.data$X, y=test.data$Y, widths=widths)
# sum(widths)
# dim(.test$basis)





