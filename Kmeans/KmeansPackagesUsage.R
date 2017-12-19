# 用R自带的Kmeans包实现Kmeans聚类

#读取iris 数据集
## 设置路径为AlgorithmPractise-R，读取当前工作目录子文件夹格式，windows格式参考 "D:\\MySpace\\机器学习\\R\\Rsourse"
iris<-read.csv("./DataSource/iris.data.csv")

#处理数据集，去掉分类数据列class
#剩余参数有四个：sepal萼片的长宽、petal 花瓣的长宽
newIris<-iris[1:4]  

#调用kmeans，将样本分为3类
kc<-kmeans(newIris,3) 


#查看
fitted(kc) #此函数用于查看之前样本点通过模型之后的拟合值，这里其实就是样本点所属分类簇质心的坐标
table(iris$class,kc$cluster) 


#聚类的可视化

#按照萼片的长宽来显示
plot(newIris[c("sepal.length","sepal.width")],
     col=kc$cluster,pch=as.integer(iris$class))

#按照花瓣的长宽来显示
plot(newIris[c("petal.length","petal.width")],
     col=kc$cluster,pch=as.integer(iris$class))

#以上参数中 col 代表颜色，这里用聚类结果kc$cluster 来显示,pch (plot charater简称)，用之前的分类来显示
#这样能清楚的看出通过聚类后的分类和之前分类的区别



