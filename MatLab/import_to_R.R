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