#install.packages("AMORE")
library(AMORE)

x1=as.numeric(c(1,1,1,1,0,0,0,0))
x2=as.numeric(c(0,0,1,1,0,1,1,0))
x3=as.numeric(c(0,1,0,1,1,0,1,0))
y=c(-1,1,1,1,-1,-1,1,-1)


p=cbind(x1,x2,x3)
target=y


net<-newff(n.neurons = c(3,2,1),learning.rate.global = 0.01,momentum.global = 0.4,
           error.criterium = "LMS",Stao = NA,hidden.layer = "tansig",output.layer = "purelin",
           method = "ADAPTgdwm")
#其中n.neurons = c(3,2,1)表示有三个输入层、2个隐藏层、1个输出层；learning.rate即为学习速率，影响了权重
#调整的精细度，太大收敛的快但是不精细。error.criterium = "LMS" 为收敛条件，lms表示最小误差平方，
#hidden.layer = "tansig",output.layer = "purelin"分别表示了隐藏层和输出层的激励函数是什么

result<-train(net,p,target,error.criterium = "LMS",report = TRUE,show.step = 100000,n.shows = 5)
#net 参数即为训练好的神经网络模型，p为学习集的自变量，target为学习集的因变量，show.step = 100000代表了重置次数

z<-sim(result$net,p)
z
