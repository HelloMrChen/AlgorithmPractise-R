
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


#拆分数据集,此函数的作用在于对于每列自变量，按照其包含的类别值将原始数据集按行拆分，以便在这个子集中计算特定自变量的熵值
splitDataSet<-function(originDataSet,axis,value){#含义即从originDataSet数据集中拆分出第axis个变量等于value的所有行，合并成子集
  retDataSet=NULL
  for (i in 1:nrow(originDataSet)) { #循环原始数据集所有行
    if(originDataSet[i,axis]==value){ #限制特定自变量，遇到目标值则记录下原始数据集整行，然后rbind行连接
      tempDataSet=originDataSet[i,]
      retDataSet=rbind(tempDataSet,retDataSet)
    }
  }
  rownames(retDataSet)=NULL
  return(retDataSet) #返回针对某个自变量的值筛选后的子集
}

#选择最佳拆分变量
chooseBestFeatureToSplita<-function(dataSet){
  bestGain=0.0
  bestFeature=-1
  baseInfo=info(dataSet) #计算总的信息熵
  numFeature<-ncol(dataSet)-1 #计算除决策变量之外的所有列，即为自变量个数 
  for (i in 1:numFeature) {#对于每个自变量计算信息熵
    featureInfo=0.0
    Feature=dataSet[,i]#定位到第i列
    classCount=levels(factor(Feature)) #计算第i列中变量类别，即有几种值
    for (j in 1:classCount) { 
    subDataSet=splitDataSet(dataSet,i,Feature[j]) #将dataSet中第i个变量等于Feature[j]的行拆分出来
    newInfo=info(subDataSet) #计算该子集的信息熵，也就是计算该变量在该取值下的信息熵部分
    prob=length(subDataSet[,1]*1.0)/nrow(dataSet)# 这里计算该变量等于Feature[j]的情况在总数据集中出现的概率
    featureInfo=featureInfo+prob*newInfo #不不断将该变量下各部分信息熵加总
    } #第第i个变量的信息熵计算结束
    
    infoGain=baseInfo-featureInfo 
    if(infoGain>bestGain){ #
      bestGain=infoGain
      bestFeature=i
    }
    
  }# 所有所有变量信息熵计算结束，并且得出了最佳拆分变量
  return(bestFeature) #返回最佳变量值
}


#最终判断属于哪一类的条件  
majorityCnt <- function(classList){  
  classCount = NULL  
  count = as.numeric(table(classList))  
  majorityList = levels(as.factor(classList))  
  if(length(count) == 1){  
    return (majorityList[1])  
  }else{  
    f = max(count)  
    return (majorityList[which(count == f)][1])  
  }  
}  

#判断剩余的值是否属于同一类，是否已经纯净了
trick <- function(classList){  
  count = as.numeric(table(classList))  
  if(length(count) == 1){  
    return (TRUE)  
  }else  
    return (FALSE)  
} 

#递归生成树
createTree<-function(dataSet){
  decision_tree = list()  
  classList = dataSet[,length(dataSet)]  
  #判断是否属于同一类  
  if(trick(classList))  
    return (rbind(decision_tree,classList[1]))  
  #是否在矩阵中只剩Label标签了，若只剩最后一列，则都分完了  
  if(ncol(dataSet) == 1){  
    decision_tree = rbind(decision_tree,majorityCnt(classList))  
    return (decision_tree)  
  } 
  
  #选择最佳属性进行分割
  bestFeature=chooseBestFeatureToSplita(dataSet)
  labelFeature=colnames(dataSet)[bestFeature] #获取最佳划分属性的变量名
  decision_tree=rbind(decision_tree,labelFeature) #这里rbind方法，如果有一个变量列数不足，会自动重复补齐
  t=dataSet[,bestFeature]
  temp_tree=data.frame()
  for(j in 1:length(levels(as.factor(t)))){  
    #这个标签的两个属性，比如“yes”，“no”，所属的数据集  
    dataSet = splitDataSet(dataSet,bestFeature,levels(as.factor(t))[j])  
    dataSet=dataSet[,-bestFeature]  
    #递归调用这个函数  
    temp_tree = createTree(dataSet)  
    decision_tree = rbind(decision_tree,temp_tree)  
  } 
  return (decision_tree)
}

t<-createTree(iris)




