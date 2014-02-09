
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

  
  
  
# data,frame version:
colnames(.data<- data.frame(y=y, x1=x1, x0=x0))
lm.1<- lm(y~x1, data=.data)
form.low<- formula(y ~ x1)
form.up<- formula(y ~ x1*x0)
scope<- list(lower=form.low, upper=form.up)
lm.2<- step(object=lm.1, direction='forward', scope=scope, steps=widths[2])
summary(lm.2)
add1(lm.1, scope=scope$upper)
