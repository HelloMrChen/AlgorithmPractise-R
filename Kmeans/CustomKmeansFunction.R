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
customKmeans<-function(dataSet,k){
  #下面要建一个记录矩阵，矩阵行数跟样本点数相同，列数为两列，第一列记录该样本点所属的类别，第二列记录误差值
  m=nrow(dataSet) #请注意，这里既代表了样本文件有几行，也代表了有几个样本点
  pointProperty=matrix(data=NA,nrow = m,ncol = 2,byrow = FALSE,dimnames = NULL) 
  
  #初始质心
  centroid=initPoints(dataSet,k)
  colnames(centroid)=colnames(dataSet)
  
  #设置递归结束位，即当质心还变化的时候，则继续递归，当然这里也可以改为变化范围的限制，即当质心只发生微小变化的时候，就停止递归
  changeFlag=TRUE
  while(changeFlag){
    changeFlag=FALSE
    for (i in 1:m) {
      #该for循环的次数为数据集的样本的个数，每行都是一个样本点，对每个样本点计算到每个质心的字段距离，判断属于那个簇，以及更新分类标志位
      minDist=Inf #预设每个样本点离所有质心的最短距离为正无穷
      minIndex=-1 #预设每个样本点的分类为-1，其实也就是初始是没有分类
      for (j in k) {
        #该for循环的循环次数为质心的个数，即计算第i个样本点到所有质心的距离，并记录下最短距离是多少，以及离哪个质心最近
        distIJ=Eudist(dataSet[i,],centroid[j,]) #计算样本中第i个点与第j个质心之间的欧式距离
        if(distIJ<minDist){
          minDist=distIJ  #每次求距离都存入minDist中，如果发现更小的距离，则替换  
          minIndex=j  #并替换该样本点所属分类为质心点的序号
        }
        
        if(pointProperty[i,1]!= minIndex) {#如果该样本点的分类发生了变化
          changeFlag=TRUE #说明质心还在变，总体分类还没趋于稳定
          pointProperty[i,1]= minIndex #更新该样本点的分类标志位
        }
        pointProperty[i,2]=minDist^2  #计算误差值 
      }
    }
    #跟新每个质心的坐标
    for(cent in 1:k){
      #该for循环的作用是找到新划分到第K个簇中的样本点，然后通过求均值求出最新的质心
     newCluster=dataSet[which(pointProperty[,1])==k,] #从数据集所有样本中拿出目前标志位中标识属于第k类的样本点
     centroid[k,]=apply(newCluster,2,mean)  #因为样本点的维度即样本点的列，所以通过计算该类的均值，计算出该类新的质心
     #通过循环后，我们重新计算了k个质心
    }
    #while函数结束，质心不再发生变化
  }
  out = list(pointProperty = pointProperty ,centroid = centroid) #将聚类中的各种分析结果按照list类型输出，当然也可以继续自定义该输出结果
  return (out)
  
#自定义kmeans函数结束
}

newIris=iris[1:4]
customKmeans(newIris,3)


