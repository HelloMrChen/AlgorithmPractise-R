
iris<-read.csv("./DataSource/iris.data.csv")
#欧式距离计算函数
Eudist<-function(x,y){
  distance<-apply(sqrt((x-y)^2),1,sum)
  return (distance)
}

#初始质心的计算
initPoints<-function(dataset,k){
#这里主要的思想是中心点的维度为原始数据集的列数，中心点的个数为要分类的个数K
  n=ncol(dataset)
  centerPointSet<-matrix(data = NA,nrow = k,ncol = n)
  for (i in 1:n){
    range=max(dataset[,i])-min(dataset[,i])
    centerPointSet[,i]=min(dataset[,i])+range*runif(k) #这里需要要转换思维，这里的min(dataset[,i])、runif(k)以及我们的结果都是k维的向量，只有range是常数
  }
  return(centerPointSet)
}

#自定义Kmeans的函数函数体
customKmeans<-function(dataSet,k,Eudist,initPoints){
  #下面要建一个记录矩阵，矩阵行数跟样本点数相同，列数为两列，第一列记录该样本点所属的类别，第二列记录误差值
  m=nrows(dataSet)
  pointProperty=matrix(data=NA,nrow = m,ncol = 2,byrow = FALSE,dimnames = NULL)
  
  #初始质心
  centroid=initPoints(dataSet,k)
  colnames(centroid)=colnames(dataSet)
  
  #设置递归结束位，即当质心还变化的时候，则继续递归，当然这里也可以改为变化范围的限制，即当质心只发生微小变化的时候，就停止递归
  
  

}



