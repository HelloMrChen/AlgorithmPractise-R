SNS<-read.csv("./DataSource/SNS.data.csv")

library(rpart)

#使用rpart包并传参数
iris.rp<-rpart(class~.,data = iris,method = "class") 

#画图
plot(iris.rp,uniform = T,branch = 0,margin = 0.1,main="iris ID3")#http://f.dataguru.cn/thread-121228-1-1.html
text(iris.rp,use.n = T,col="blue",cex=1.2) #use.n 是控制下边50/0/0样本分类概况，col字体颜色、cex 字体大小


