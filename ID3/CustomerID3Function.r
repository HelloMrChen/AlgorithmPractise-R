
#用R语言实现决策树ID3算法，以iris数据集为例

#计算总体信息值的函数,这里只允许最后一列作为决策结果列
info<-function(dataSet){
  rowCount=nrow(dataSet) #计算数据集中有几行，也即有几个样本点
  colCount=ncol(dataSet)
  resultClass=NULL
  resultClass=levels(factor(dataSet[,colCount]))  #此代码取得判别列中有个可能的值，输出  "Iris-setosa"     "Iris-versicolor" "Iris-virginica" 
  classCount=NULL
  classCount[resultClass]=rep(0,length(resultClass)) #以决策变量的值为下标构建计数数组，用于计算和存储样本中出现相应变量的个数
  
  for(i in 1:rowCount){ #该for循环的作用是计算决策变量中每个值出现的个数，为计算信息值公式做准备
    if(dataSet[i,colCount] %in% resultClass){
      temp=dataSet[i,colCount]
      classCount[temp]=classCount[temp]+1
    }
  }
  
  #计算总体的信息值
  t=NULL
  info=0
  for (i in 1:length(resultClass)) {
    t[i]=classCount[i]/rowCount
    info=-t[i]*log2(t[i])+info
  }
  return(info)
}


