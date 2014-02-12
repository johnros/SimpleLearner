#### Playing with model.matrix construction ####

x.p<- 5
x<- matrix(rnorm(10000),1000,x.p, dimnames=list(NULL, LETTERS[1:x.p]))
colnames(x.framed<- as.data.frame(x))
colnames(.xx<- model.matrix(terms(x=formula(~.^10), data=x.framed), data=x.framed))
y<- .xx %*% runif(ncol(.xx), 0, 30)  + rnorm(nrow(.xx), sd=2)
widths<- rep(5,10)
names( .data<- data.frame(y, x) )

lm(y~2:4, data=.data)



# envir() version 
vars<- new.env()
assign(x='x1', value=x1, envir=vars)
assign(x='x0', value=x0, envir=vars)
assign(x='y', value=y, envir=vars)

(lm.1<- lm(y~x1, data=vars))
resid<- residuals(lm.1)
form.low<- formula(y ~ x1, env=vars)
form.up<- formula(y ~ x1*x0, env=vars)
scope<- list(lower=form.low, upper=form.up)
lm.2<- step(object=lm.1, direction='forward', scope=scope, steps=widths[2])
summary(lm.2)
add1(lm.1, scope=scope$upper )
drop1(lm.1)
anova(lm.2)

  
  
  
# data.frame version:
colnames(.data<- data.frame(y=y, x1=x1, x0=x0))
lm.1<- lm(y~x1, data=.data)
form.low<- formula(y ~ x1)
form.up<- formula(y ~ x1*x0)
scope<- list(lower=form.low, upper=form.up)
lm.2<- step(object=lm.1, direction='forward', scope=scope, steps=widths[2])
summary(lm.2)
add1(lm.1, scope=scope$upper)






#### Test-train model selection ####
library(e1071)
model.1<- svm(y~.^3, data=data.framed, type='C', kernel='linear', cross=5, cost=1000)
summary(model.1)
form.low<- formula(kmScore~1)





### e1071:::tune examples 
data(iris)
## tune `svm' for classification with RBF-kernel (default in svm),
## using one split for training/validation set

obj <- tune(svm, Species~., data = iris, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))
length(obj$train.ind)

## alternatively:
control<-   tune.control(sampling = "fix")
obj <- tune.svm(Species~., data = iris, gamma = 2^(-1:1), cost = 2^(2:4), tunecontrol=control)
length(obj$train.ind)

control<-   tune.control(sampling = "cross")
obj <- tune.svm(Species~., data = iris, gamma = 2^(-1:1), cost = 2^(2:4), tunecontrol=control)
length(obj$train.ind)

control<-   tune.control(sampling = "boot")
obj <- tune.svm(Species~., data = iris, gamma = 2^(-1:1), cost = 2^(2:4), tunecontrol=control)
length(obj$train.ind)


summary(obj)
plot(obj)



#### Equivalence of variable selection method ####
## Make data:
n<- 100
p<- 10
x.raw<- as.data.frame(matrix(rnorm(n*p, sd=1),n, p, dimnames=list(NULL, LETTERS[1:p])))
x<- t(apply(x.raw, 1, `*`, seq(length.out=ncol(x.raw)))) # Each predictor has different variance
coefs<- runif(p, 0, 1)
y<- as.matrix(x) %*% coefs   + rnorm(n, sd=1)
.data<- data.frame(y=y, x=x)
lm.0<- lm(y~x.A, data=.data)
## Check invariance of correlation:
x0<- cbind(x[,'A'], 1)
x0.svd<- svd(x0)
U<- x0.svd$u
x.orth<- as.matrix( x- U%*%t(U) %*% as.matrix(x))
round(t(x.orth) %*% x0, 10) # Check ortogonalization worked

(cors<- cor(residuals(lm.0), x.orth)[1,])
# Note the correlation with A (which is in the model) is not zero. Numerics?!?

# Which are clearly different than:
cor(y, x.orth)
cor(residuals(lm.0), x)
cor(y, x)



## Is the ordering in agreement with RSS? Yup!
form.up.expr<- sprintf("~%s", paste(colnames(.data)[-1], collapse="+"))
add1.0<- add1(lm.0, scope=as.formula(form.up.expr) , data=.data, k=0)
# Result:
cbind(
  rownames(add1.0)[order(add1.0$RSS, decreasing=FALSE)],
  names(cors)[order(abs(cors), decreasing=TRUE)])

