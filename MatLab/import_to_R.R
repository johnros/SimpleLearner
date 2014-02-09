#### Import data ####
library(R.matlab)

data<- readMat('../MatLab/data.mat')
class(data)
ls.str(data)
attributes(data)
data.framed<- data.frame(y=data$Y,data$X)
names(data.framed)
dim(data.framed)


#### Test forware search approach ####
library(e1071)
model.1<- svm(y~.^3, data=data.framed, type='C', kernel='linear',cost=100)
summary(model.1)
form.low<- formula(kmScore~1)
