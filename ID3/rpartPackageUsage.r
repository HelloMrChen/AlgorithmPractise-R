SNS<-read.csv("./DataSource/SNS.data.csv")

library(rpart)

iris.rp<-rpart(class~.,data = iris,method = "class")
plot(iris.rp,uniform = T,branch = 0,margin = 0.1,main="iris ID3")
text(iris.rp,use.n = T,col="blue",cex=0.9)

fancyRpartPlot(iris.rp)
