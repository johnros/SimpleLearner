

## Create control parameters 
makeControl<- function(
  sampling="fix",
  penalty=0
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
  #     x.p<- 5
  #     x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
  #     x.framed<- as.data.frame(x)
  #     .xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed)
  #     y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
  #     widths<- rep(4,10)
  #     control<- makeControl()
  
  ## Checks:
  if(any(widths > head(c(1,widths) * ncol(x),-1))) stop("Impossible width value.")
  if(missing(control)) control<- makeControl() 
  if(is.factor(y)) y<- as.numeric(as.character(y))
  
  ## Initializing 
  n<- nrow(x)
  p<- ncol(x)
  
  ## Make first layer:
  x<- cbind(1, x)
  x0.svd<- svd(x)
  W<- x0.svd$v[ , 1:widths[1]]
  x1 <- x %*% W # Low dimensional X representation (F in Ohad)
  
  ## Make next layers:
  # Check for orthogonality with existing layers
  # Check for predictive power
  .data<- data.frame(y, x1) # Note this will remove column names
  colnames(.data)<- sprintf("V%d", 1:ncol(.data))
  lm.1<- lm(V1~., data=.data)
  
  
  for(i in 2:length(widths)){
    #i<- 2
    form.low<- formula(lm.1)
    x<- model.matrix(lm.1)
        
    x.candid.inds<- rep(1:ncol(x), each=ncol(x1))
    x1.candid.inds<- rep(1:ncol(x1), times=ncol(x))
    ## TODO: manage column names to create predictor
    x.candidate<- x[,x.candid.inds] * x1[,x1.candid.inds]
    
    .data<- data.frame(y, x, x.candidate)
    colnames(.data)<- sprintf("V%d", 1:ncol(.data))
    
    x.high.ind<- 2:ncol(x.candidate)
    form.up.expression<- paste('V1 ~',paste(sprintf("V%d", x.high.ind), collapse="+"))
    form.up <- as.formula(form.up.expression)
    
    lm.1<- step(object=lm.1, scope=list(lower=form.low, upper=form.up), direction='forward', steps=widths[i])
  }
  anova(lm.1)
    
  ## TODO: return rotation matrix and variable construction to allow prediction on new data
  return(list(
    basis= model.matrix(lm.1)[,-1],
    svd=x0.svd,
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





